function get_lastfm() {
    const url = "https://api.piturnah.xyz/lastfm-current";
    fetch(url)
        .then((response) => response.text())
        .then((text) => {
            let result = JSON.parse(text).recenttracks.track[0];
            console.log(result);
            const field = document.getElementById("currently-listening");

            if (result["@attr"]?.nowplaying === "true") {
                field.innerHTML = `I'm currently listening to <a href="${result.url}">${result.name}</a>, by ${result.artist["#text"]}.`;
            } else {
                field.innerHTML = `The last track I listened to was <a href="${result.url}">${result.name}</a> by ${result.artist["#text"]}, ${timeSince(new Date(result.date.uts * 1000))} ago.`;
            }
        });
}

// Adapted from <https://stackoverflow.com/questions/3177836/how-to-format-time-since-xxx-e-g-4-minutes-ago-similar-to-stack-exchange-site>.
function timeSince(date) {

  var seconds = Math.floor((new Date() - date) / 1000);

  var interval = seconds / 31536000;

  if (interval > 1) {
    interval = Math.floor(interval);
    if (interval == 1) {
        return Math.floor(interval) + " year";
    } else {
        return Math.floor(interval) + " years";
    }
  }
  interval = seconds / 2592000;
  if (interval > 1) {
    interval = Math.floor(interval);
    if (interval == 1) {
        return Math.floor(interval) + " month";
    } else {
        return Math.floor(interval) + " months";
    }
  }
  interval = seconds / 86400;
  if (interval > 1) {
    interval = Math.floor(interval);
    if (interval == 1) {
        return Math.floor(interval) + " day";
    } else {
        return Math.floor(interval) + " days";
    }
  }
  interval = seconds / 3600;
  if (interval > 1) {
    interval = Math.floor(interval);
    if (interval == 1) {
        return Math.floor(interval) + " hour";
    } else {
        return Math.floor(interval) + " hours";
    }
  }
  interval = seconds / 60;
  if (interval > 1) {
    interval = Math.floor(interval);
    if (interval == 1) {
        return Math.floor(interval) + " minute";
    } else {
        return Math.floor(interval) + " minutes";
    }
  }
  return Math.floor(seconds) + " seconds";
}

get_lastfm();
