#!/bin/sh
echo "syncing..."
aws s3 sync --acl public-read --sse --delete ./static/ s3://spacedmath.com
echo "done"
