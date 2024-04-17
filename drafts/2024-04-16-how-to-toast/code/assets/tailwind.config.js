module.exports = {
  content: [
    "../lib/**/*.*ex"
  ],
  theme: {
    extend: {
      animation: {
        popup: 'popup 0.25s ease-out'
      }
    }
  },
  plugin: [
    require("@tailwindcss/forms")
  ]
}
