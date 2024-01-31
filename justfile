fmt:
  sbt scalafmtAll

fix:
  sbt 'scalafmtAll; scalafixAll'

clean:
  git clean -xdf

check-deps:
  sbt dependencyUpdates

test:
  sbt 'test'

watch:
  sbt '~compile'

publish-local:
  sbt 'publishLocal'
