include karax/prelude

setRenderer proc: VNode =
  buildHtml body:
    tdiv:
      text "Hello world!"
