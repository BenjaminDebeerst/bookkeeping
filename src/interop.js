export const flags = ({ env }) => {
  return localStorage.getItem('storage') || ""
}

export const onReady = ({ app, env }) => {
  if (app.ports && app.ports.save) {
    app.ports.save.subscribe(storage => {
      localStorage.setItem('storage', storage)
    })
  }
}
