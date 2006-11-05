JsonRpcRequestId = 1;

JsonRpcTransaction = Class.create();
Object.extend(JsonRpcTransaction.prototype,
{
    initialize: function(serviceUrl, methodName, params) {
	this.serviceUrl = serviceUrl;
	this.methodName = methodName;
	this.params = params;
	this.reply = null;
	this.replyReady = 0;
	this.callbacks = [];
	new Ajax.Request(serviceUrl,
			 { method: 'post',
			   requestHeaders: ['Content-type', 'application/json',
					    'Accept', 'application/json'],
			   postBody: JSON.stringify({ version: "1.1",
						      id: JsonRpcRequestId++, // yaws needs this
						      method: methodName,
						      params: params }),
			   onComplete: this.receiveReply.bind(this) });
    },

    receiveReply: function(ajaxRequest) {
	var reply = JSON.parse(ajaxRequest.responseText).result;
	this.reply = reply;
	this.replyReady = 1;
	this.callbacks.each(function (cb) { cb(reply); });
    },

    addCallback: function(cb) {
	this.callbacks.push(cb);
	if (this.replyReady) {
	    cb(this.reply);
	}
	return this;
    }
});

JsonService = Class.create();
Object.extend(JsonService.prototype,
{
    initialize: function(smdUrl) {
	this.smdUrl = smdUrl;
	var smdRequest = new Ajax.Request(smdUrl, { method: 'get',
						    asynchronous: false });
	this.smd = JSON.parse(smdRequest.transport.responseText);
	// ^ the SMD contains objectName, serviceURL, methods, etc etc.

	var svc = this;
	this.smd.methods.each(function (desc) {
				  svc[desc.name] = svc.makeGenericProxy(desc);
			      });
    },

    makeGenericProxy: function(desc) {
	return function () {
	    var params = $A(arguments);
	    return new JsonRpcTransaction(this.smd.serviceURL,
					  desc.name,
					  params);
	};
    }
});
