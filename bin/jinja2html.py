#!/usr/bin/env python

import fnmatch, json, os, re, sys, xml.dom.minidom

from jinja2 import Environment, FileSystemLoader

def main(args):
    env = Environment(loader=FileSystemLoader(
            os.path.join(os.path.dirname(os.path.dirname(sys.argv[0])),
                         'doc', 'templates')))
    template = env.get_template(args[1])
    with open(args[2], 'w') as outfile:
        outfile.write(template.render())

if __name__ == '__main__':
    main(sys.argv)
