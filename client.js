var jsonrpc = imprt("jsonrpc");

var jb = new jsonrpc.ServiceProxy("jukebox.yaws", [
				      "login",
				      "whoami",
				      "logout",
				      "search",
				  ]);

function update_username(jbResp) {
    document.getElementById('username').value = jbResp[0];
}

function change_username() {
    update_username(jb.login(document.getElementById('username').value));
}

function do_logout() {
    update_username(jb.logout('dummy'));
}

function do_search() {
    var keys = document.getElementById("searchtext").value.split(/ +/);
    jb.search(keys);
}

function initClient() {
    update_username(jb.whoami('dummy'));
}
