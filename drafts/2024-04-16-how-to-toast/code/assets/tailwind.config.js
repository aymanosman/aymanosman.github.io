module.exports = {
  content: [
    "./js/**/*.js",
    "../lib/**/*.*ex"
  ],
  theme: {
    extend: {
      animation: {
        popup: 'popup 0.25s ease-out',
        fadedown: 'fadedown 0.25s ease-out'
      }
    }
  },
  plugin: [
    require("@tailwindcss/forms")
  ]
}
