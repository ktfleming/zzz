{
  offset = {
    minutes = +540 -- minutes from UTC (can be negative or positive)
  , summerOnly = False -- test
  },
  keymap = {
    -- Immediately exits the program
    quit = {
      key = "c",
      modifiers = ["ctrl"]
    },
    
    -- When pressed while editing a model via a form, persists that
    -- model to disk and exits the form
    save = {
      key = "s",
      modifiers = ["ctrl"]
    },

    -- Navigate up the hierarchy when viewing models, or exit a form
    -- without saving changes
    back = {
      key = "esc",
      modifiers = [] : List Text
    },

    -- Display the form for adding a model
    add = {
      key = "a",
      modifiers = ["ctrl"]
    },

    -- Display the form for editing the selected model
    edit = {
      key = "e",
      modifiers = ["ctrl"]
    },

    -- Delete the currently displayed model
    delete = {
      key = "d",
      modifiers = ["ctrl"]
    },

    -- Cancel an active request
    cancel = {
      key = "x",
      modifiers = ["ctrl"]
    },

    -- Send a request, or select an item in a list
    submit = {
      key = "enter",
      modifiers = [] : List Text
    },

    -- Select the next widget on a page with multiple
    focusNext = {
      key = "tab",
      modifiers = [] : List Text
    },

    -- Select the previous widget on a page with multiple
    focusPrev = {
      key = "backtab",
      modifiers = [] : List Text
    },

    -- Scroll the response body display up, or navigate up
    -- in a list
    scrollUp = {
      key = "up",
      modifiers = [] : List Text
    },

    -- Scroll the response body display down, or navigate down
    -- in a list
    scrollDown = {
      key = "down",
      modifiers = [] : List Text
    },

    -- Displays the help screen
    showHelp = {
      key = "p",
      modifiers = ["ctrl"]
    },

    -- Displays a screen from which you can search all defined
    -- environments, projects, and request definitions
    searchAll = {
      key = "f",
      modifiers = ["ctrl"]
    },

    -- Displays the environment list screen, from which environments
    -- can be selected, added, and edited
    showEnvironments = {
      key = "u",
      modifiers = ["ctrl"]
    }

  }
}
