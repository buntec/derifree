fmt:
  sbt scalafmtAll

fix:
  sbt 'scalafmtAll; scalafixAll'

clean:
  git clean -xdf

check-deps:
  sbt dependencyUpdates

test:
  sbt 'derifree/test'

watch:
  sbt '~derifree/compile'

publish-local:
  sbt 'derifree/publishLocal'
