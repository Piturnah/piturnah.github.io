/** @type {import('tailwindcss').Config} */
export default {
  content: ["./src/**/*.{html,js,svelte,ts}"],
  theme: {
    extend: {},
    colors: {
      grey: {
        200: "#1C1D19",
        600: "#75715E",
      },
      pink: "#F92672",
      yellow: "#E6DB74",
      blue: "#66D9EF",
    },
    fontFamily: {
      montserrat: ["Montserrat", "sans-serif"],
    },
  },
  plugins: [],
};
