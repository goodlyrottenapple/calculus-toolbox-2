export default () => {
  const template = [
    {
      label: 'File',
      submenu: [
        {
          label: "Open...",
          accelerator: 'CmdOrCtrl+O',
          click: function (item, focusedWindow) {
          // tell the focused window to tell the app to open the config
            focusedWindow.send('menu:open')
          }
        },
        {
          label: "Save",
          accelerator: 'CmdOrCtrl+S',
          click: function (item, focusedWindow) {
            focusedWindow.send('menu:save')
          }
        },
        {
          label: "Save As...",
          accelerator: 'Shift+CmdOrCtrl+S',
          click: function (item, focusedWindow) {
            focusedWindow.send('menu:saveAs')
          }
        },
        {
          label: "Add/Modify Calculus",
          click: function (item, focusedWindow) {
            focusedWindow.send('menu:edit')
          }
        }
      ]
    },
    {
      label: 'Edit',
      submenu: [
        {
          role: 'undo'
        },
        {
          role: 'redo'
        },
        {
          type: 'separator'
        },
        {
          role: 'cut'
        },
        {
          role: 'copy'
        },
        {
          role: 'paste'
        },
        {
          role: 'pasteandmatchstyle'
        },
        {
          role: 'delete'
        },
        {
          role: 'selectall'
        }
      ]
    },
    {
      label: 'View',
      submenu: [
        {
          label: 'Reload',
          accelerator: 'CmdOrCtrl+R',
          click (item, focusedWindow) {
            if (focusedWindow) focusedWindow.reload()
          }
        },
        {
          label: 'Toggle Developer Tools',
          accelerator: window.process.platform === 'darwin' ? 'Alt+Command+I' : 'Ctrl+Shift+I',
          click (item, focusedWindow) {
            if (focusedWindow) focusedWindow.webContents.toggleDevTools()
          }
        },
        {
          type: 'separator'
        },
        {
          role: 'resetzoom'
        },
        {
          role: 'zoomin'
        },
        {
          role: 'zoomout'
        },
        {
          type: 'separator'
        },
        {
          role: 'togglefullscreen'
        }
      ]
    },
    {
      role: 'window',
      submenu: [
        {
          role: 'minimize'
        },
        {
          role: 'close'
        }
      ]
    },
    {
      role: 'help',
      submenu: [
        {
          label: 'Learn More',
          click () { window.require('electron').shell.openExternal('http://electron.atom.io') }
        }
      ]
    }
  ]

  if (window.process.platform === 'darwin') {
      const name = '' //app.getName()
      template.unshift({
        label: name,
        submenu: [
          {
            role: 'about'
          },
          {
            type: 'separator'
          },
          {
            role: 'services',
            submenu: []
          },
          {
            type: 'separator'
          },
          {
            label: "Preferences",
            click: function (item, focusedWindow) {
            // tell the focused window to tell the app to open the config
              focusedWindow.send('menu:prefs')
            }
          },
          {
            type: 'separator'
          },
          {
            role: 'hide'
          },
          {
            role: 'hideothers'
          },
          {
            role: 'unhide'
          },
          {
            type: 'separator'
          },
          {
            role: 'quit'
          }
        ]
      })
      // Edit menu.
      template[2].submenu.push(
        {
          type: 'separator'
        },
        {
          label: 'Speech',
          submenu: [
            {
              role: 'startspeaking'
            },
            {
              role: 'stopspeaking'
            }
          ]
        }
      )
      // Window menu.
      template[4].submenu = [
        {
          label: 'Close',
          accelerator: 'CmdOrCtrl+W',
          role: 'close'
        },
        {
          label: 'Minimize',
          accelerator: 'CmdOrCtrl+M',
          role: 'minimize'
        },
        {
          label: 'Zoom',
          role: 'zoom'
        },
        {
          type: 'separator'
        },
        {
          label: 'Bring All to Front',
          role: 'front'
        }
      ]
    } else {
      template[2].submenu.push(
        {
          label: "Preferences",
          click: function (item, focusedWindow) {
          // tell the focused window to tell the app to open the config
            focusedWindow.send('menu:prefs')
          }
        }
      )
    }

    return template;
} 