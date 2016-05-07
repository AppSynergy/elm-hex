gulp = require 'gulp'
watch = require 'gulp-watch'
exec = require('child_process').exec

gulp.task 'default', ['testwatch', 'examplewatch']

gulp.task 'examplewatch', () ->
  gulp.watch '{src/**,examples/**}', ['example']

gulp.task 'testwatch', () ->
  gulp.watch '{src/**,tests/**}', ['test']

gulp.task 'example', () ->
  exec 'elm make examples/Example.elm --output=build/index.html', showme

gulp.task 'test', () ->
  exec 'elm-test tests/TestRunner.elm', showme

showme = (err, stdout, stderr) ->
  console.log stdout
  console.log stderr
