// some variables
var activeCardId;

var cardWidth = 0;
var cardHeight = 0;

function setupCardSize() {
	var card = $$('.card')[0];
	if (card) {
		cardHeight = card.getHeight();
		cardWidth = card.getWidth();
	}
}

// setup on load
Event.observe(window, 'load', function() {
	setupCardSize();
});

function toggleCard(cardId) {
	var cardElem = $(cardId);
	if(cardElem.hasClassName("active")) {
		collapseCard(activeCardId);
		activeCardId = null;
	} else {
		setActiveCard(cardId);
	}
}

function setActiveCard(cardId) {
	if (activeCardId !== cardId) {
		collapseCard(activeCardId);
		expandCard(cardId);
		activeCardId = cardId;
	}
}

function expandCard(cardId) {
	var cardElem = $(cardId);
	if (cardElem) {
		//show more adornments
		cardElem.addClassName('active');
	
		new Effect.Morph(cardId, {
			style: 'width: ' + (cardWidth*2+16) + 'px; height: ' + cardHeight + 'px;', // CSS Properties
  			duration: 0.8
		}); 
		new Effect.Shake(cardId, {
			duration: 1,
			distance: 5
		});
		console.log("expand: " + cardId);
	}
}

function collapseCard(cardId) {
	var cardElem = $(cardId);
	if (cardElem) {
		//hide some adornments
		cardElem.removeClassName('active');
	
		new Effect.Morph(cardId, {
			style: 'width: ' + cardWidth + 'px; height: ' + cardHeight + 'px;', // CSS Properties
  			duration: 0.8
		}); 
		new Effect.Shake(cardId, {
			duration: 1,
			distance: 2
		});
		console.log("collapse: " + cardId);
	}
}


// setup sortable stuff
Event.observe(window, 'load', function() {

	// only sortable when no filter is active
	if($('dataProvision').value + $('contributorRole').value + $('contributor').value == "ALLALLALL") {
		Sortable.create('canvas', {
			tag: 'div',
			onUpdate: updateCardPriority,
			onChange: updateCardPriority,
		});
		$('canvas').setStyle("cursor: move;")
	} else {
		var prio = $('cardPriority');
		prio.parentNode.removeChild(prio);
	}
});
		
function updateCardPriority() {
	var priorityList = "";
	cards = $$('.card');
	for (i = 0; i < cards.size(); i++) {
		priorityList += cards[i].id + ";";
	}
	$('cardPriority').value = priorityList;
}
		

var callbackStack = {};
var dialogIdCounter = 0;
function getUserInput(message, title, callback, modal, resizable) {
	if(typeof resizable == "undefined") resizable = false;
	if(typeof modal == "undefined") modal = true;
	if(typeof callback != "function") {
		callbackStack[++dialogIdCounter] = function() {$j(this).dialog("destroy");};
	}
	else {
		callbackStack[++dialogIdCounter] = callback;
	}
	$j('<div id="dlg'+dialogIdCounter+'">'+message+'</div>').dialog({"title": title, buttons: {"OK": function() { var id = $j(this).attr("id").substr(3); var callback = window.callbackStack[id]; callback(this); $j(this).dialog("destroy"); delete window.callbackStack[id]; }, "Abbrechen": function() {$j(this).dialog("destroy");}}, minHeight: 0, "modal": modal, "resizable": resizable, show: "slide"});
} 
var userControlIdCounter = 0;
function generateUserInputControl(label, name, value, style, type, attributes) {
	if(typeof style == "undefined") style = "";
	if(typeof value == "undefined") value = "";
	if(typeof type == "undefined") type = "text";
	var id = name+ userControlIdCounter++;
	var html = '<label for="'+id+'">'+label+'</label> <input type="'+type+'" name="'+name+'" style="'+style+'" id="'+id+'" value="'+value+'" ';
	if(attributes) for(k in attributes) { html += k+'="'+attributes[k]+'" ' }
	return html + "/><br/>";
}

function createNewAlphaCard(caseId) {
	getUserInput(generateUserInputControl("Titel", "title"), "Neue AlphaCard anlegen", function(dlg) {
		var data = {};
		data.saveCard = true;
		data["alphaCardIdentifier.caseId"] = caseId;
		data["alphaCardIdentifier.cardId"] = "";
		data["alphaCardDescriptor.title"] = $j(dlg).find("input[name='title']:first").val();
		if(data["alphaCardDescriptor.title"] == "") return;
		
		var html = '<form action="cardform" method="post" accept-charset="iso-8859-1">';
		for(k in data) html += '<input type="hidden" name="'+k+'" value="'+data[k]+'" />';
		html += '</form>'; 
		$j(html).appendTo("body").submit();
			
	});
}

function showCardRename(cardId, caseId, currentTitle) {
	getUserInput(generateUserInputControl("AlphaCard Titel", "title", currentTitle), "AlphaCard umbenennen", function(dlg) {
		var data = {};
		data.saveCard = true;
		data["alphaCardIdentifier.caseId"] = caseId;
		data["alphaCardIdentifier.cardId"] = cardId;
		data["alphaCardDescriptor.title"] = $j(dlg).find("input[name='title']:first").val();
		if(data["alphaCardDescriptor.title"] == "") return;
		
		var html = '<form action="cardform" method="post" accept-charset="iso-8859-1">';
		for(k in data) html += '<input type="hidden" name="'+k+'" value="'+data[k]+'" />';
		html += '</form>'; 
		$j(html).appendTo("body").submit();
			
	});
}