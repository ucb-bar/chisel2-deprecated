#!/usr/bin/env python

import fnmatch, json, os, re, sys, xml.dom.minidom

from jinja2 import Environment, FileSystemLoader

def get_element_text(node):
    result = ""
    for text in node.childNodes:
        if text.nodeType == xml.dom.minidom.Node.TEXT_NODE:
            result += text.data.strip()
    return result


def load_test_results(path):
    """Load junitResult.xml as outputed by Jenkins"""
    doc = xml.dom.minidom.parse(path)
    timestamp = get_element_text(
        doc.documentElement.getElementsByTagName("timestamp")[0])
    nb_tests = len(doc.documentElement.getElementsByTagName("case"))
    nb_failures = len(
        doc.documentElement.getElementsByTagName("errorStackTrace"))
    return timestamp, nb_tests, nb_failures

def load_checkstyle_violations(path):
    """Load violations.xml as outputed by Jenkins"""
    nb_violations = 0
    doc = xml.dom.minidom.parse(path)
    look = re.match('.*/(\d\d\d\d-\d\d-\d\d)_(\d\d)-(\d\d)-(\d\d)/.*', path)
    if look:
        timestamp = "%sT%s:%s:%s-0700" % (
            look.group(1), look.group(2), look.group(3), look.group(4))
        for node in doc.documentElement.getElementsByTagName("file"):
            nb_violations += int(node.getAttribute("count"))
        return timestamp, nb_violations
    raise ValueError("cannot extract timestamp for %s" % path)


def load_coverage(path):
    """Load coverage.xml as outputed by Jenkins"""
    doc = xml.dom.minidom.parse(path)
    look = re.match('.*/(\d\d\d\d-\d\d-\d\d)_(\d\d)-(\d\d)-(\d\d)/.*', path)
    if look:
        timestamp = "%sT%s:%s:%s-0700" % (
            look.group(1), look.group(2), look.group(3), look.group(4))
        line_coverage = float(doc.documentElement.getAttribute("line-rate"))
        return timestamp, line_coverage
    raise ValueError("cannot extract timestamp for %s" % path)


def main(args):
    failures = []
    coverage = []
    violations = []
    root_dir = os.getcwd()
    for root, dirnames, filenames in os.walk(root_dir):
        # unit test results
        for path in fnmatch.filter(filenames, 'junitResult.xml'):
            timestamp, nb_tests, nb_failures = load_test_results(
                os.path.join(root, path))
            failures += [ (timestamp, float(nb_failures) / nb_tests) ]
        # checkstyle violations
        for path in fnmatch.filter(filenames, 'violations.xml'):
            timestamp, nb_violations = load_checkstyle_violations(
                os.path.join(root, path))
            violations += [ (timestamp, nb_violations) ]
        # line coverage
        for path in fnmatch.filter(filenames, 'coverage.xml'):
            timestamp, line_coverage = load_coverage(
                os.path.join(root, path))
            coverage += [ (timestamp, line_coverage) ]

    json_compile_data = json.dumps(
        [{ 'key': 'checkstyle violations',
           'values': sorted(violations, key=lambda test: test[0]) }],
        indent=2, sort_keys=True)
    json_runtime_data = json.dumps(
        [{ 'key': '% test failures',
           'values': sorted(failures, key=lambda test: test[0]) },
         { 'key': '% line coverage',
           'values': sorted(coverage, key=lambda test: test[0]) } ],
        indent=2, sort_keys=True)

    env = Environment(loader=FileSystemLoader(os.path.dirname(sys.argv[0])))
    template = env.get_template('unit_tests_trends.html')
    print template.render(compile_data=json_compile_data,
                          runtime_data=json_runtime_data)

if __name__ == '__main__':
    main(sys.argv)
