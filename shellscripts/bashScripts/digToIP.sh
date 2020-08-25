#!/bin/bash

dig $* | awk '{if($4 == "A") print $5}'
