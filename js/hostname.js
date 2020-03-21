var window = window || {};

// modified from: https://stackoverflow.com/questions/8498592/extract-hostname-name-from-string
window.getHostname = (uri) => {
  const x = $1.match(/^(?:https?:)?(?:\/\/)?([^\/\?]+)/i);
  return (x && x[1] || '');
}
