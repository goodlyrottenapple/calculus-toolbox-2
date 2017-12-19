const electron = require('electron');
const child_process = require('child_process')
// Module to control application life.
const app = electron.app;
// Module to create native browser window.
const BrowserWindow = electron.BrowserWindow;

const path = require('path');
const url = require('url');
const appRootDir = require('app-root-dir').get();

const production = process.env.ELECTRON_START_URL === undefined

const Store = require('electron-store');
const store = new Store();

// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let mainWindow;
// Do the same for the backend web server.
let backendServer

function createWindow() {
    // Create the browser window.
    mainWindow = new BrowserWindow({titleBarStyle: 'hidden-inset', width: 800, height: 600, show: false});

    // and load the index.html of the app.

    const startUrl = process.env.ELECTRON_START_URL || url.format({
        pathname: path.join(__dirname, '/../build/index.html'),
        protocol: 'file:',
        slashes: true
    });
    mainWindow.loadURL(startUrl);

    // Open the DevTools.
    // if(production) 
      mainWindow.webContents.openDevTools();

    const {ipcMain} = require('electron')
    ipcMain.on('updateMacros', e => {
      console.log('updateMacros received');
      mainWindow.webContents.send('updateMacros2');
    });

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
  const execPath = production ?
    path.join(path.dirname(appRootDir), 'bin'):
    path.join(appRootDir, 'resources');

  const cmd = path.join(execPath, 'calculus-toolbox');
  // backendServer = child_process.spawn(cmd, ['gui', '--path', '../calculi', '--name' , 'Sequent']);
  backendServer = child_process.spawn(cmd, ['gui', '--path', '../calculi']);

  backendServer.stdout.on('data', (data) => {
    console.log(`stdout: ${data}`);
  });

  backendServer.stderr.on('data', (data) => {
    console.log(`stderr: ${data}`);
  });
}

// Start the backend web server when Electron has finished initializing.
app.on('ready', createBackendServer)
// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.on('ready', createWindow);

app.on('will-quit', function() {
  backendServer.kill()
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

// In this file you can include the rest of your app's specific main process
// code. You can also put them in separate files and require them here.