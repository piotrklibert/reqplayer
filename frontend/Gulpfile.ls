require! {
  fs, del, gulp,
  'merge2': stream-merge
  'gulp-preprocess': preprocess
  'gulp-livescript': lscript
  'gulp-browserify': browserify
  'gulp-concat': concat
  'gulp-cssmin': cssmin
  'gulp-rename': rename
  'gulp-uglify': uglify
}

const TARGET = "../priv/"

s = gulp~src
d = gulp~dest

gulp.task "clean", ->
  s(TARGET).pipe del


gulp.task "copy-index", ->
  s("./index.html")
    .pipe d(TARGET+"html/")


gulp.task "copy-statics", ->
  const stream = stream-merge(
    s("./assets/img/**", base: "./assets/"),
    s("./node_modules/**", base: ".")
  )
  stream.pipe d(TARGET)

gulp.task "build-styles", ->
  s("./assets/styles/*.css")
    .pipe concat("main.css")
    .pipe d(TARGET+"styles/")

gulp.task "build-scripts", ->
  s("./src/scripts.ls")
    .pipe lscript(bare: true)
    .pipe browserify!
    .pipe rename("bundle.js")
    .pipe d(TARGET + "scripts/")

gulp.task "watch", ->
  gulp.watch("./assets/styles/*.css", ["build-styles"])
  gulp.watch("./src/*.ls", ["build-scripts"])
  gulp.watch("./index.html", ["copy-index"])

gulp.task "default", [
  "copy-index",
  "copy-statics",
  "build-scripts",
  "build-styles"
]
