const es_week = 1;
const css_week =1;

document.addEventListener("DOMContentLoaded", () => {
  const allLinks = document.querySelectorAll('nav a');

  allLinks.forEach((link) => {
    const href = link.getAttribute("href");
    if (!href) return;

    console.log("Found link:", href);

    // Check for experimental sociology links
    if (href.includes("experimental-sociology/week")) {
      const match = href.match(/week(\d{2})/i);
      if (match) {
        const weekNum = parseInt(match[1], 10);
        if (weekNum > es_week) {
          console.log(`Hiding ES link for week ${weekNum}`);
          link.closest("li")?.style.setProperty("display", "none", "important");
        }
      }
    }

    // Check for computational social sciences links
    if (href.includes("computational-social-sciences/week")) {
      const match = href.match(/week(\d{2})/i);
      if (match) {
        const weekNum = parseInt(match[1], 10);
        if (weekNum > css_week) {
          console.log(`Hiding CSS link for week ${weekNum}`);
          link.closest("li")?.style.setProperty("display", "none", "important");
        }
      }
    }
  });
});
