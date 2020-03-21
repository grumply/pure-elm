var window = window || {};

// modified from: https://stackoverflow.com/questions/8498592/extract-hostname-name-from-string
window.getHostname = (url) => {
  const x = url.match(/^(?:https?:)?(?:\/\/)?([^\/\?]+)/i);
  return (x && x[1] || '');
}

// modified from: https://stackoverflow.com/questions/31430167/regex-check-if-given-string-is-relative-url
window.isRelative = (url) => {
  const re = RegExp('^(?!www\.|(?:http|ftp)s?://|[A-Za-z]:\\|//).*','gmi');
  return re.test(url)
}
