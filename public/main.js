const app = Elm.Main.init({
  flags: localStorage.getItem('storage')
})

app.ports.save.subscribe(storage => {
  localStorage.setItem('storage', storage)
  app.ports.load.send(storage)
})