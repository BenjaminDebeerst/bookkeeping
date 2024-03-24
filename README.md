# Bookkeeping

Simple, privacy-first, personal accounting software.

Import bank statements from one or more bank accounts (via CSVs), categorize statements and get monthly overviews of your income and spending.

This web-application runs client-side only. No cloud, no data sent anywhere, keeping your sensitive data under your own control. Data is stored in the browser local storage but can be saved to and loaded from files as well.

## Installation

Prerequisites are working installations of npm and elm.

Install the npm dependencies, then run locally via

    npx elm-land server

You can now access the application at `http://localhost:1234`.

Of course you can use elm-spa to package the application and deploy it to a server of your choice as well.

## Development

Follow instructions for Installation, then the elm-land server will auto-build upon change and report errors. Bookkeeping is written entirely in Elm with the [Elm-Land](https://elm.land) framework.
