#!/bin/bash

echo 'public class Test {'
echo '  static void ensure(boolean b) {}'
sed  's/^/  /' $1
echo '}'
