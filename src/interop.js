export const flags = ({ env }) => {
  return localStorage.getItem('storage') || ""
}

export const onReady = ({ app, env }) => {
  if (app.ports && app.ports.save) {
    app.ports.save.subscribe(storage => {
      console.log("Received data from Elm", storage)
      localStorage.setItem('storage', storage)
    })
  }
}
