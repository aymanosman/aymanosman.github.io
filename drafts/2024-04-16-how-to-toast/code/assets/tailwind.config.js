const plugin = require("tailwindcss/plugin");

module.exports = {
  content: [
    "./js/**/*.js",
    "../lib/**/*.*ex"
  ],
  plugin: [
    require("@tailwindcss/forms")
  ]
}
