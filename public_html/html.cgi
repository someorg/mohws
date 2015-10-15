#! /bin/bash
echo Content-Type: text/html
echo
echo '<html>'
echo '<head>'
echo '<title>Hello World</title>'
echo '</head>'
echo '<body>'
echo '<ul>'
echo '<li>Hello World</li>'
echo '<li>'`date`'</li>'
echo '<li>Query: '$QUERY_STRING'</li>'
echo '<li>Port: '$SERVER_PORT'</li>'
echo '</ul>'
echo '</body>'
echo '</html>'
