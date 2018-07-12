#!/bin/sh

Counter=1
## Wait for the test LeoFS cluster up
until docker-compose up --abort-on-container-exit; [ $? -ne 0 ]; do
    echo "${Counter}th test passed."
    Counter=$(expr $Counter + 1)
    sleep 5
done
echo "${Counter}th test failed with code $?."
