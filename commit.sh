#!/bin/sh
git add --all
timestamp() {
  date +"at %H:%M:%S on %d/%m/%Y"
}
git commit -am "Auto commit at $(timestamp)"
git push origin gh-pages