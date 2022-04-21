const {remote,contextBridge} =  require('electron');
const {BrowserWindow} = remote;
const path = require('path');
const createNewWindow = () =&gt; {
    let win = new BrowserWindow({
        width: 1000,
        height: 800,
        webPreferences: {
            nodeIntegration: false,
            enableRemoteModule: true,
            contextIsolation: true,
            preload: path.join(__dirname, 'preload.js')
        },
    });
    win.loadFile("index.html");
}
contextBridge.exposeInMainWorld('testapi', {
    createNewWin: createNewWindow,
  }
)