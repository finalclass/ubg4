#!/bin/bash

docker build -t finalclass/ubg4 .
docker push finalclass/ubg4:latest

ssh fc1 "fc-ubg sync && fc-ubg restart"
