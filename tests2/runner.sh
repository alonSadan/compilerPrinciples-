#!/bin/sh

for test in $(ls $1);
do
    test_name=${test%.*}
    # echo "Test: $test_name"
    make $1/$test_name >> /dev/null
    o1=`scheme -q < $1/$test`
    o2=`./$test_name`
    echo "(equal? '($o1) '($o2))" > test.scm
    result=`scheme -q < test.scm`;
    echo "Test $test_name Result: $result"
    rm $test_name
    rm "$1/$test_name.o"
    rm "$1/$test_name.s"
done
rm "test.scm"
