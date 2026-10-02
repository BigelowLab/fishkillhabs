#!/bin/bash
# Copy daily risk maps to cloud storage bucket for delivery to web platform

gcloud storage rsync --recursive /mnt/ecocast/projects/fishkillhabs/predictions/ gs://habs/predictions/

