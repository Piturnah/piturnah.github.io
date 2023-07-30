/** @type {import('tailwindcss').Config} */
export default {
  content: ["./src/**/*.{html,js,svelte,ts}"],
  theme: {
    extend: {},
    colors: {
      grey: {
        200: "#1C1D19",
        300: "#272822",
        600: "#75715E",
      },
      pink: "#F92672",
      yellow: "#E6DB74",
      blue: "#66D9EF",
    },
    fontFamily: {
      montserrat: ["Montserrat", "sans-serif"],
    },
    backgroundImage: {
      background: "url('/background.png')",
    },
    backgroundSize: {
      "60%": "60%",
    },
    dropShadow: {
      "3xl": "0.06em 0.06em 10px #000000",
    },
  },
  plugins: [],
};
