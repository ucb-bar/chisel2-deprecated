#!/usr/bin/env python

import fnmatch, json, os, re, sys, xml.dom.minidom, time

from jinja2 import Environment, FileSystemLoader

def get_element_text(node):
    result = ""
    for text in node.childNodes:
        if text.nodeType == xml.dom.minidom.Node.TEXT_NODE:
            result += text.data.strip()
    return result

def load_build_info(path):
    """Load build.xml as output by Jenkins"""
    doc = xml.dom.minidom.parse(path)
    timestampElements = doc.documentElement.getElementsByTagName("timestamp")
    assert timestampElements.length == 1
    timestampStr = get_element_text(timestampElements[0])
    localTimestamp = time.localtime(float(timestampStr)/1000)
    GMTOffset = time.altzone if localTimestamp.tm_isdst else time.timezone
    # print "timestamp: %s\n" % (timestampStr)
    sign = ""
    if GMTOffset < 0:
        sign = '-'
        GMTOffset = -GMTOffset
    else:
        sign = '+'
    hourOffset = GMTOffset/3600
    minOffset = (GMTOffset - (hourOffset * 3600))/60
    hhmmOffset = '{}{:02d}{:02d}'.format(sign, hourOffset, minOffset)
    timestamp = time.strftime("%Y-%m-%dT%H:%M:%S", localTimestamp) + hhmmOffset
    return timestamp

def load_test_results(path):
    """Load junitResult.xml as output by Jenkins"""
    doc = xml.dom.minidom.parse(path)
    nb_tests = len(doc.documentElement.getElementsByTagName("case"))
    nb_failures = len(
        doc.documentElement.getElementsByTagName("errorStackTrace"))
    return nb_tests, nb_failures

def load_checkstyle_violations(path):
    """Load violations.xml as output by Jenkins"""
    nb_violations = 0
    doc = xml.dom.minidom.parse(path)
    for node in doc.documentElement.getElementsByTagName("file"):
        nb_violations += int(node.getAttribute("count"))
    return nb_violations

def load_coverage(path):
    """Load coverage.xml as output by Jenkins"""
    doc = xml.dom.minidom.parse(path)
    line_coverage = float(doc.documentElement.getAttribute("line-rate"))
    return line_coverage


def main(args):
    failures = []
    coverage = []
    violations = []
    root_dir = os.path.join(os.getcwd(), 'builds')
    for root, dirnames, filenames in os.walk(root_dir):
        if not ('build.xml' in filenames):
            continue
        # General build info
        timestamp = load_build_info(os.path.join(root, 'build.xml'))
        # unit test results
        for path in fnmatch.filter(filenames, 'junitResult.xml'):
            nb_tests, nb_failures = load_test_results(
                os.path.join(root, path))
            failures += [ (timestamp, float(nb_failures) / nb_tests) ]
        # checkstyle violations
        for dir in fnmatch.filter(dirnames, 'violations'):
            path = os.path.join(root, dir, 'violations.xml')
            nb_violations = load_checkstyle_violations(path)
            violations += [ (timestamp, nb_violations) ]
        # line coverage
        for path in fnmatch.filter(filenames, 'coverage.xml'):
            line_coverage = load_coverage(
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

    env = Environment(loader=FileSystemLoader(
            os.path.join(os.path.dirname(os.path.dirname(sys.argv[0])),
                         'doc', 'templates')))
    template = env.get_template('unit_tests_trends.html')
    print template.render(compile_data=json_compile_data,
                          runtime_data=json_runtime_data)

if __name__ == '__main__':
    main(sys.argv)
