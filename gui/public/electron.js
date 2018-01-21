const electron = require('electron');
const child_process = require('child_process')
// Module to control application life.
const app = electron.app;
const ipcMain = electron.ipcMain;

// Module to create native browser window.
const BrowserWindow = electron.BrowserWindow;

const path = require('path');
const url = require('url');
const appRootDir = require('app-root-dir').get();

const production = process.env.ELECTRON_START_URL === undefined

const Store = require('electron-store');
const store = new Store();
const portfinder = require('portfinder');
let port;


// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let mainWindow;
// Do the same for the backend web server.
let backendServer


function urlPath(p) {
  var editUrl;
  if (process.env.ELECTRON_START_URL) {
    if (process.platform === "win32") editUrl = process.env.ELECTRON_START_URL + "#" + p
    else editUrl = path.join(process.env.ELECTRON_START_URL , "#", p)
  }
  else
    editUrl = url.format({
      pathname: path.join(__dirname, '/../build/index.html'),
      hash: p,
      protocol: 'file:',
      slashes: true
    });
  return editUrl;
};


function createWindow() {
    // Create the browser window.
    mainWindow = new BrowserWindow({titleBarStyle: 'hidden-inset', width: 800, height: 600, show: false});
    mainWindow.port = port

    // and load the index.html of the app.

    const startUrl = urlPath('/');
    mainWindow.loadURL(startUrl);


    // Open the DevTools.
    if(!production) 
      mainWindow.webContents.openDevTools();


    // Emitted when the window is closed.
    mainWindow.on('closed', function () {
        // Dereference the window object, usually you would store windows
        // in an array if your app supports multi windows, this is the time
        // when you should delete the corresponding element.
        
        mainWindow = null
    })

    mainWindow.on('ready-to-show', function() { 
      mainWindow.show(); 
      mainWindow.focus(); 
    });
}

function createBackendServer () {
  // if running a dubug server in ghci, launch without spawning a process
  if(process.env.DEBUG_PORT) {
    port = process.env.DEBUG_PORT
    return createWindow()
  }

  const execPath = production ?
    path.join(path.dirname(appRootDir), 'bin'):
    path.join(appRootDir, 'resources');

  const exe = process.platform === 'win32' ? ".exe" : ""
  const cmd = path.join(execPath, 'calculus-toolbox' + exe);
  var workDir = store.get('workDir')

  const launch = () => {
    portfinder.getPortPromise()
    .then((p) => {
      port = p
      console.log("launching on port: " + port)
      console.log("launching in workDir: " + workDir)
      const currentCalc = store.get('currentCalc')
      if(currentCalc && currentCalc !== '') backendServer = child_process.spawn(cmd, ['gui', '--port', port, '--path', workDir, '--calc', currentCalc]);
      else backendServer = child_process.spawn(cmd, ['gui', '--port', port, '--path', workDir]);

      backendServer.stdout.on('data', (data) => {
        console.log(`stdout: ${data}`);
      });

      backendServer.stderr.on('data', (data) => {
        console.log(`stderr: ${data}`);
      });
      createWindow(port);
    })
    .catch((err) => {
      alert(err);
    });
  }

  if(workDir === undefined || workDir === ''){
    var setupWindow = new BrowserWindow({ width: 800, height: 300, title:"Modify calculus" });

    setupWindow.loadURL(urlPath('/preferences/initial'));
    setupWindow.setMenu(null);

    setupWindow.on('close', function () {
      setupWindow = null
      workDir = store.get('workDir')
      if(workDir && workDir !== '') launch()
      else app.quit();
    })      
  } else launch() 
}

// Start the backend web server when Electron has finished initializing.
// This method will be called when Electron has finished
app.on('ready', createBackendServer)
// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
// app.on('ready', createWindow);

app.on('will-quit', function() {
  if(backendServer) backendServer.kill()
})

// Quit when all windows are closed.
app.on('window-all-closed', function () {
    // On OS X it is common for applications and their menu bar
    // to stay active until the user quits explicitly with Cmd + Q
    if (process.platform !== 'darwin') {
        app.quit()
    }
});

app.on('activate', function () {
    // On OS X it's common to re-create a window in the app when the
    // dock icon is clicked and there are no other windows open.
    if (mainWindow === null) {
        createWindow()
    }
});

ipcMain.on('updateMacros', e => {
  console.log('updateMacros received');
  mainWindow.webContents.send('updateMacros2');
});

ipcMain.on('menu:edit', e => {
  var win = new BrowserWindow({ width: 800, height: 700, title:"Modify calculus" });
  win.port = port
  win.loadURL(urlPath('/edit/'));
  win.setMenu(null);
  win.on('closed', function () {
    win = null
  })
})
// In this file you can include the rest of your app's specific main process
// code. You can also put them in separate files and require them here.