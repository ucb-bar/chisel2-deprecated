#!/usr/bin/env python

# Prerequisites on OSX:
#
# $ port install py27-euca2ools
# $ pip install -U boto Fabric
#
# export AWS_ACCOUNT_ID=<your_account_id_on_aws>
# (optional)
#   export SMTP_USER=<smtp_user>
#   export SMTP_PASSWORD=<smtp_password>

import os, smtplib, sys, time
from email.mime.text import MIMEText

import boto, boto.ec2
import fabric.api as fab
import fabric.exceptions
from fabric.decorators import task

#EC2_BUNDLE_VOL    = 'euca-bundle-vol-2.7'
#EC2_UPLOAD_BUNDLE = 'euca-upload-bundle-2.7'
EC2_BUNDLE_VOL    = os.path.join('.', 'bin', 'ec2-bundle-vol')
EC2_UPLOAD_BUNDLE = os.path.join('.', 'bin', 'ec2-upload-bundle')

fab.env.update({
        'user': 'ubuntu',
        'key_filename': os.path.join(
            os.getenv('HOME'), '.ssh', 'ec2-aspire-key')
})

key_name = os.path.basename(fab.env.get('key_filename'))
AWS_ACCOUNT_ID = os.getenv('AWS_ACCOUNT_ID')
x509_name = 'chisel.eecs.berkeley.edu'

SMTP_USER = os.getenv('SMTP_USER')
SMTP_PASSWORD = os.getenv('SMTP_PASSWORD')

def wait_for_public_dns_name(conn, reserv):
    counter = 0
    result = None
    sys.stdout.write("Waiting for public dns name to come up ...\n")
    while counter < len(reserv.instances):
        time.sleep(1)
        filtered = conn.get_all_instances(
            instance_ids=[str(inst.id) for inst in reserv.instances])
        counter = 0
        result = filtered[0]
        for rsv in filtered:
            for inst in rsv.instances:
                if len(inst.public_dns_name) > 0:
                    counter = counter + 1
    return result


@task
def create_credentials():
    """
    Create credentials to remotely loging into EC2 instances (key pair)
    """
    ec2 = boto.connect_ec2()
    key_path = fab.env.get('key_filename')
    if not os.path.exists(key_path):
        # only needs to be done once
        key_pair = ec2.create_key_pair(key_name)
        key_pair.save(os.path.dirname(key_path))


@task
def install():
    """
    Start a stock Ubuntu image on EC2, then install the tools necessary
    to develop with Chisel on it.
    """
    conn = boto.ec2.connect_to_region('us-west-2')
    # Ubuntu 12.04LTS us-west-2 amd64 instance-store
    # (http://cloud-images.ubuntu.com/locator/ec2/)
    reservation = conn.run_instances(
        image_id='ami-6435a954',
        key_name=key_name)

    # Wait a minute or two while it boots
    reservation = wait_for_public_dns_name(conn, reservation)

    # Install prerequisite tools
    fab.env['host_string'] = reservation.instances[0].public_dns_name
    sys.stdout.write("%s\n" % fab.env['host_string'])
    time.sleep(10) # waiting for ssh daemon to come up.
    fab.sudo('apt-get update')
    fab.sudo('DEBIAN_FRONTEND=noninteractive apt-get -y install '\
             ' openjdk-7-jdk git-core build-essential libtool'\
             ' gawk libgmp-dev libmpfr-dev libmpc-dev bisonc++ flex autoconf'\
             ' python-software-properties emacs iverilog unzip ruby')
    fab.sudo('add-apt-repository -y ppa:ubuntu-toolchain-r/test')
    fab.sudo('apt-get update')
    fab.sudo('DEBIAN_FRONTEND=noninteractive apt-get -y install gcc-4.8 g++-4.8')
    fab.sudo('update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.6 60 --slave /usr/bin/g++ g++ /usr/bin/g++-4.6')
    fab.sudo('update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.8 40 --slave /usr/bin/g++ g++ /usr/bin/g++-4.8')
    fab.sudo('update-alternatives --set gcc /usr/bin/gcc-4.8')
    fab.run('wget http://scalasbt.artifactoryonline.com/scalasbt/sbt-native-packages/org/scala-sbt/sbt/0.13.0/sbt.tgz')
    fab.run('tar zxvf sbt.tgz')
    fab.sudo('mv sbt/bin/* /usr/local/bin')
    fab.run('rm -rf sbt*')
    fab.run('git clone https://github.com/ucb-bar/chisel.git')
    fab.run('git clone https://github.com/ucb-bar/chisel-tutorial.git')
    fab.run('git clone https://github.com/ucb-bar/riscv-sodor.git')
    with fab.cd("chisel-tutorial"):
        fab.run('make check')
        fab.run('make clean')
    sys.stdout.write("%s configured.\n" % fab.env['host_string'])


@task
def package(public_dns_name):
    """
    Package the instance as an AMI, make it available publicly
    and return a ami-id.

    Note: Ruby will need to be present on the system in order to use
    Amazon AMI tools.
    """
    fab.env['host_string'] = public_dns_name
    conn = boto.ec2.connect_to_region('us-west-2')
    x509_private_key_path = os.path.join(
        os.getenv('HOME'), '.ssh', x509_name + '.key')
    x509_cert_path = os.path.join(
        os.getenv('HOME'), '.ssh', x509_name + '.crt')
    cert_dir = "/tmp/cert"
    username = fab.env.get('user')
    s3_bucket = 'chisel-bucket/ami-001'
    manifest_path = "/tmp/image.manifest.xml"
    ec2_home = os.path.join(cert_dir, 'ec2-ami-tools-1.4.0.9')
    ec2_access_key = boto.config.get_value('Credentials', 'aws_access_key_id')
    ec2_secret_key = boto.config.get_value(
        'Credentials', 'aws_secret_access_key')
    fab.run("mkdir -p %s" % cert_dir)
    fab.run("chmod 777 %s" % cert_dir)
    fab.local("scp -i %s %s %s %s@%s:%s"
              % (fab.env.get('key_filename'),
                 x509_private_key_path, x509_cert_path,
                 username, public_dns_name, cert_dir))
    with fab.cd(cert_dir):
        fab.run("wget http://s3.amazonaws.com/ec2-downloads/ec2-ami-tools.zip")
        fab.run("unzip ec2-ami-tools.zip")
        with fab.cd(ec2_home):
            with fab.shell_env(
                EC2_HOME=ec2_home,
                EC2_ACCESS_KEY=ec2_access_key,
                EC2_SECRET_KEY=ec2_secret_key):
                #s = Server.create_from_instance_id(
                #    "i-0000000", "My Server Name")
                #b = s.get_bundler().bundle()
                # Add --ec2cert for Eucalyptus
                fab.sudo("%(ec2_bundle_vol)s -k %(private_keyfile)s -c %(certificate_file)s -u %(user_id)s -e %(cert_dir)s -r x86_64"
                         % { "ec2_bundle_vol": EC2_BUNDLE_VOL,
                             "private_keyfile": os.path.join(
                            cert_dir, os.path.basename(x509_private_key_path)),
                             "certificate_file": os.path.join(
                            cert_dir, os.path.basename(x509_cert_path)),
                             "user_id": AWS_ACCOUNT_ID,
                             "cert_dir": cert_dir})
                # From the docs, if the bucket doesn't exist and the name
                # is available, it will be created.
                # XXX It seems ec2-upload-bundle does not like environment
                # variables.
                fab.run("%(cmd)s --access-key %(ec2_access_key)s --secret-key %(ec2_secret_key)s -b %(s3_bucket)s -m %(manifest_path)s"
                    % { "cmd": EC2_UPLOAD_BUNDLE,
                        "ec2_access_key": ec2_access_key,
                        "ec2_secret_key": ec2_secret_key,
                        "s3_bucket": s3_bucket,
                        "manifest_path": manifest_path })

    # XXX This is necessary when we override the s3_bucket.
    # conn.deregister_image('ami-c4821df4')
    image_id = conn.register_image(
        name='chisel-ami-public',
        description="Chisel installed on Ubuntu 12.04 LTS",
        image_location=os.path.join(s3_bucket, os.path.basename(manifest_path)))
    sys.stdout.write("ami: %s\n" % image_id)
    # Make the ami public to all
    conn.modify_image_attribute(image_id,
        attribute='launchPermission', operation='add', groups=['all'])
    return image_id


@task
def deploy(email_passwd_list, send_confirmation=False):
    """
    Start as many number of Chisel-ready EC2 instances as necessary
    and associate them one-to-one to the identifers list.
    """
    nb_instances = len(email_passwd_list)
    sys.stdout.write("deploy %d instances\n" % nb_instances)
    conn = boto.ec2.connect_to_region('us-west-2')
    # Chisel AMI
    reservation = conn.run_instances(
        image_id='ami-345ec104',
        min_count=nb_instances,
        max_count=nb_instances,
        key_name=key_name)

    # Wait a minute or two while it boots
    reservation = wait_for_public_dns_name(conn, reservation)

    for identifier, instance in zip(email_passwd_list, reservation.instances):
        fab.env['host_string'] = instance.public_dns_name
        counter = 0
        while counter < 3:
            try:
                fab.run("echo")
            except fabric.exceptions.NetworkError:
                time.sleep(20)
            counter = counter + 1
        fab.sudo('echo "ubuntu:%s" | chpasswd' % identifier[1])
        fab.sudo('cp /etc/ssh/sshd_config /etc/ssh/sshd_config~')
        fab.sudo("sed -e 's/PasswordAuthentication no/PasswordAuthentication yes/' /etc/ssh/sshd_config~ > /etc/ssh/sshd_config")
        fab.sudo("restart ssh")

    for identifier, instance in zip(email_passwd_list, reservation.instances):
        sys.stdout.write("%s %s" % (identifier[0], instance.public_dns_name))
        if send_confirmation:
            try:
                msg = MIMEText("""
Welcome to the [Chisel](https://chisel.eecs.berkeley.edu) bootcamp!

To access the Amazon EC2 instance you will be used for the lab exercise,
please follow these instructions:

1. Login to your machine using your email when prompted for a password
    $ ssh ubuntu@%(public_dns_name)s

2. Go to the chisel-tutorial
    $ cd chisel-tutorial

3. Read through the README and wait for the lab session to start

Thank you,
- The Chisel Development Team.
""" % {"public_dns_name": instance.public_dns_name})
                msg['Subject'] = 'Welcome to the Chisel bootcamp!'
                msg['From'] = 'Chisel <parlab-admin@eecs.berkeley.edu>'
                msg['To'] = identifier[0]
                server = smtplib.SMTP('smtp.gmail.com', 587)
                server.ehlo()
                server.starttls()
                server.login(SMTP_USER, SMTP_PASSWORD)
                server.sendmail('parlab-admin@eecs.berkeley.edu',
                                [msg['To']], msg.as_string())
                server.quit()
                sys.stdout.write(" email sent OK")
            finally:
                sys.stdout.write("\n")


@task
def teardown():
    """
    Tearn down the instances previously created with *setup*.

    Be careful! This will terminate all instances associated
    with the account. We rely on the fact we use ec2 only
    for chisel bootcamp here.
    """
    conn = boto.ec2.connect_to_region('us-west-2')
    reserv = conn.get_all_instances()
    for rsv in reserv:
        conn.terminate_instances([ inst.id for inst in rsv.instances])


def main(args):
    if args[1] == 'create_credentials':
        create_credentials()
    elif args[1] == 'install':
        install()
    elif args[1] == 'package':
        package(args[2])
    elif args[1] == 'deploy':
        email_list = args[2:]
        if os.path.exists(args[2]):
            with open(args[2]) as email_file:
                email_list = email_file.readlines()
        email_passwd_list = []
        for email_passwd in email_list:
            email_passwd_list += [ email_passwd.strip().split(':') ]
        deploy(email_passwd_list, send_confirmation=False)
    elif args[1] == 'teardown':
        teardown()
    else:
        sys.stderr.write("error: [%s] unknown command '%s'"
                         % (args[0], args[1]))


if __name__ == '__main__':
    main(sys.argv)
