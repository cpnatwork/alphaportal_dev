<%@ page language="java" pageEncoding="utf-8" contentType="text/html;charset=utf-8"%>
<%@ include file="/common/taglibs.jsp"%>
<%@ taglib uri="http://alpha.portal/functions" prefix="apf" %>

<head>
	<title><fmt:message key="case.title" /></title>
	
	<link rel="stylesheet" type="text/css" href="<c:url value='/styles/jquery-ui-1.8.16.custom.css'/>" />
	<script type="text/javascript" src="<c:url value='/scripts/jquery-1.6.2.min.js'/>"></script>
	<script type="text/javascript" src="<c:url value='/scripts/jquery-ui-1.8.16.custom.min.js'/>"></script>
	<script type="text/javascript">var $j = jQuery.noConflict();</script>
	
	<link rel="stylesheet" type="text/css" href="/styles/alphaportal.css" />
	<script type="text/javascript" src="/scripts/alphaPortalCaseView.js"></script>
</head>

<style>
.toggableBox {
  margin-bottom: 1.5em;
}

.cardState_open {
  background-color: red !important;
}

.cardState_inprogress {
  background-color: blue !important;
}

.cardState_fullfilled {
  background-color: lime !important;
}

</style>
<script>
Event.observe(window, 'load', function() {
	setActiveCard('${activeCard.alphaCardIdentifier.cardId}');
});
</script>

 <%-- show some create case form --%>
<c:if test="${not empty case && empty case.caseId}">
	<div class="leftBox">
		<h1><fmt:message key='case.heading'/></h1>
		<form:form commandName="case" method="post" action="caseform" id="caseForm" onsubmit="return validateCase(this)">
	
		<div>
			<appfuse:label styleClass="desc" key="case.name"/>
	   		<form:errors path="name" cssClass="fieldError"/>
	   	    <form:input path="name" id="name" cssClass="text medium" cssErrorClass="text medium error" maxlength="50"/>
   	    </div>
   	    <br />
   	    <div class="buttonBar bottom">
   	   	    <input type="submit" class="button right" name="addCase" value="<fmt:message key='button.save'/>" />
   	    </div>
   		</form:form>
   	</div>
</c:if>

 <%-- show the actual cardform --%>
<c:if test="${not empty case && not empty case.caseId}">

	<div class="rightBox">
		<fmt:message key='case.changeView'/>:&nbsp;
		<c:if test="${empty isMyWorklist}">
			<a href="<c:url value='/caseform?caseId=${case.caseId}&isMyWorklist=1'/>" style="font-weight: bold;"><fmt:message key='case.view.myworklist'/></a>
		</c:if>
		<c:if test="${not empty isMyWorklist}">
			<a href="<c:url value='/caseform?caseId=${case.caseId}&dataProvision=ALL&contributorRole=ALL&contributor=ALL&showDeleted=NOTDELETED'/>" style="font-weight: bold;"><fmt:message key='case.view.casedocuments'/></a>
		</c:if>
	</div>

 	<%-- heading stuff (worklist or not)--%>
	<c:if test="${empty isMyWorklist}">
		<h1><fmt:message key='case.heading'/>&nbsp;-&nbsp;${case.name}&nbsp;<sup class="edit" onclick="Effect.toggle('caseTitleForm', 'blind');"><fmt:message key='text.edit'/></sup></h1>

		<div id="caseTitleForm" class="leftbox toggableBox" style="display: none;">
			<form:form commandName="case" method="post" action="caseform" id="caseTitleForm" onsubmit="return validateCase(this)">
				<form:hidden path="caseId" />
				<ul>
					<li>
						<appfuse:label styleClass="desc" key="case.name"/>
				   	    <form:input path="name" id="name" cssClass="text medium" cssErrorClass="text medium error" maxlength="50"/>
					</li>
	   		    	<li class="buttonBar bottom" style="min-height: 1em;">
   	   		    		<input type="submit" class="button left" name="saveCase" value="<fmt:message key='button.save'/>"/>
   	    			</li>
	   		    </ul>
   	    	</form:form>
	   	</div>
	</c:if>
	<c:if test="${not empty isMyWorklist}">
		<h1><fmt:message key='case.heading.myworklist'/>&nbsp;-&nbsp;${case.name}</h1>
	</c:if>

	<br style="clear: left;"/>
	
	<%--Filter --%>
	<c:if test="${empty isMyWorklist}">
		<div>
   	 	  	<form:form commandName="filters" method="get" action="caseform" id="filterForm">
   	 	  		<input type="hidden" name="caseId" value="${case.caseId}" />
   	 	  		<c:if test="${not empty activeCard}">
   	 	  			<input type="hidden" name="activeCardId" value="${activeCard.alphaCardIdentifier.cardId}" />
   	 	  		</c:if>
   	 	  		<form:select path="dataProvision" onchange="this.form.submit()">
     				<form:options items="${dataProvision}" itemLabel="name" />
   	 	  		</form:select>
   	 	  		<form:select path="contributorRole" onchange="this.form.submit()">
     				<form:options items="${contributorRole}" itemLabel="name" />
   	 	  		</form:select>
   	 	  		<form:select path="contributor" onchange="this.form.submit()">
     				<form:options items="${contributor}" itemLabel="name" />
   	   			</form:select>
   	   			<form:select path="showDeleted" onchange="this.form.submit()">
     				<form:options items="${showDeleted}" itemLabel="name" />
   	   			</form:select>
   		   	</form:form>
	   	 </div>
   	</c:if>
   	 
   	<%-- the cards --%>

	<div id="canvas">
		<c:forEach items="${cards}" var="card" varStatus="loopstatus">
			<div id="${card.alphaCardIdentifier.cardId}" class="card cardState_${apf:dataProvisionFromCard(card)}">
				<form method="post" action="cardform" id="cardForm_${card.alphaCardIdentifier.cardId}" acceptCharset="iso-8859-1">
					<input type="hidden" name="alphaCardIdentifier.cardId" value="${card.alphaCardIdentifier.cardId}"/>
					<input type="hidden" name="alphaCardIdentifier.caseId" value="${card.alphaCardIdentifier.caseId}"/>
					<div class="innercard">
						<div class="content">
							<div class="cardheader" onClick="toggleCard('${card.alphaCardIdentifier.cardId}')">${card.alphaCardDescriptor.title}
								<c:if test="${not apf:isCardMarkedAsDeleted(card)}">
									<img onclick="showCardRename('${card.alphaCardIdentifier.cardId}', '${card.alphaCardIdentifier.caseId}', '${card.alphaCardDescriptor.title}')"
										src="/styles/icons/edit-rename.png" alt="<fmt:message key='text.edit'/>" style="float: right; cursor:pointer;"/>
								</c:if>
							</div>
							<div class="main">
								<c:forEach items="${card.alphaCardDescriptor.allAdornments}" var="ad">
									<div class="row<c:if test="${apf:contains(essentialAdornments, ad.name) }"> essential</c:if>">
										<div class="name">
											<a href="adornmentform?id=${ad.adornmentId}&card=${card.alphaCardIdentifier.cardId}&case=${card.alphaCardIdentifier.caseId}">
												<c:out value="${ad.name}"/>
											</a>
							 			</div>
										<div class="value">
											<a href="adornmentform?id=${ad.adornmentId}&card=${card.alphaCardIdentifier.cardId}&case=${card.alphaCardIdentifier.caseId}"><c:out value="${ad.value}"/></a>
										</div>
									</div>
								</c:forEach>
							</div>
							<div class="buttonBar cardButtons">
						
								<c:if test="${not empty card.alphaCardIdentifier.cardId}">
									<c:if test="${not empty card.alphaCardDescriptor.contributor and apf:isUserContributor(currentUserId, card)}">
				    	    			<c:if test="${not apf:isCardMarkedAsDeleted(card)}">
				    	    				<input class="cardButton" type="submit" style="background-image: url(/styles/icons/list-add.png);" value="" title="<fmt:message key="button.addAdornment"/>" onclick="location.href='<c:url value="adornmentform?card=${card.alphaCardIdentifier.cardId}&case=${card.alphaCardIdentifier.caseId}"/>'; return false;"/>
				    	    			</c:if>
									</c:if>
				
				
									<c:if test="${not apf:isCardMarkedAsDeleted(card)}">
										<c:choose>
											<c:when test="${not apf:isUserContributor(currentUserId, card) and apf:contains(currentUserContributorRoles, card.alphaCardDescriptor.contributorRole)}">
												<input class="cardButton" type="submit" style="background-image: url(/styles/icons/user-new.png);" name="assignToMe" value="" title="<fmt:message key="button.assignToMe"/>" />
											</c:when>
											<c:otherwise>
												<c:if test="${apf:isUserContributor(currentUserId, card)}">
													<input class="cardButton" type="submit" style="background-image: url(/styles/icons/user-delete.png);" name="unassignMe" value="" title="<fmt:message key="button.unassignMe"/>"/>
												</c:if>
											</c:otherwise>
										</c:choose>
									</c:if>
									
									<c:if test="${empty card.alphaCardDescriptor.contributor || apf:isUserContributor(currentUserId, card)}">
										<c:if test="${not apf:isCardMarkedAsDeleted(card)}">
											<input class="cardButton" type="submit" style="background-image: url(/styles/icons/user-group-new.png);"
												onclick="location.href='<c:url value="/cardassignform?card=${card.alphaCardIdentifier.cardId}&case=${card.alphaCardIdentifier.caseId}"/>'; return false;"
												value="" title="<fmt:message key="button.assignOther"/>"/>
										</c:if>
									</c:if>
				
									<c:if test="${apf:isUserContributor(currentUserId, card) or apf:adornmentValueFromCard('Sichtbarkeit', card) == 'Öffentlich' or apf:adornmentValueFromCard('Verantwortlicher', card) == ''}">
										<c:if test="${not empty card.payload}">
											<input class="cardButton" type="submit" style="background-image: url(/styles/icons/download.png);" name="payloadGet" value="" title="<fmt:message key="button.payload"/>"/>
											
											<c:if test="${apf:isUserContributor(currentUserId, card)}">
												<c:if test="${not apf:isCardMarkedAsDeleted(card)}">
													<input class="cardButton" type="submit" style="background-image: url(/styles/icons/document-new.png);"
														onclick="location.href='<c:url value="/cardfileupload?card=${card.alphaCardIdentifier.cardId}&case=${card.alphaCardIdentifier.caseId}"/>'; return false;"
														value="" title="<fmt:message key="button.addNewPayload"/>"/>
												</c:if>
												<input class="cardButton" type="submit" style="background-image: url(/styles/icons/document-open-recent.png);"
													 onclick="location.href='<c:url value="/payloadVersions?card=${card.alphaCardIdentifier.cardId}&case=${card.alphaCardIdentifier.caseId}"/>'; return false;"
													value="" title="<fmt:message key="button.payloadVersions"/>"/>
											</c:if>
										</c:if>
				
										<c:if test="${empty card.payload && not apf:isCardMarkedAsDeleted(card)}">
											<input class="cardButton" type="submit" style="background-image: url(/styles/icons/document-new.png);"
														onclick="location.href='<c:url value="/cardfileupload?card=${card.alphaCardIdentifier.cardId}&case=${card.alphaCardIdentifier.caseId}"/>'; return false;"
												value="" title="<fmt:message key="button.addPayload"/>"/>
										</c:if>
									</c:if>
									
									<c:choose>
										<c:when test="${not apf:isCardMarkedAsDeleted(card) && apf:isUserContributor(currentUserId, card)}">
											<input class="cardButton" type="submit" style="background-image: url(/styles/icons/edit-delete.png);" name="setDeleted" value=""  title="<fmt:message key="button.setACardDeleted"/>" />
										</c:when>
										<c:otherwise>
											<c:if test="${apf:isUserContributor(currentUserId, card)}">
												<input class="cardButton" type="submit" style="background-image: url(/styles/icons/view-refresh.png);" name="setNotDeleted" value="" title="<fmt:message key="button.setACardNotDeleted"/>"/>
											</c:if>
										</c:otherwise>
									</c:choose>
									
				    	    	</c:if>						
						
							</div>
						</div>
					</div>
			 	</form>
			</div>
		</c:forEach>
	</div>

	<br style="clear: left;"/>

	<div class="buttonBar bottom">
		<form:form commandName="case" method="post" action="caseform" id="caseForm" onsubmit="return validateCase(this)">
			<form:errors path="*" cssClass="error" element="div" />
			<form:hidden path="caseId" />
			<input type='hidden' name="cardPriority" id="cardPriority" />
			<c:if test="${not empty case.caseId}">
				<input type="button" class="button" style="margin-right: 5px"
					onclick="createNewAlphaCard('${case.caseId}')"
					value="<fmt:message key='button.addCard'/>" />
			</c:if>
			<input type="button" class="button" onclick="location.href='cardVersionHistory?case=${case.caseId}'" value="<fmt:message key="button.versionhistory"/>"/>
			<c:if test="${not empty case.caseId && empty isMyWorklist}">
				<input type="submit" class="button right" name="cancelCase" value="<fmt:message key='button.cancel'/>" onclick="bCancel=true" />
		   		<input type="submit" class="button right" name="saveCase" value="<fmt:message key='button.save'/>" />
	   		</c:if>
   		</form:form>		
	</div>
	<br />

	<%-- case details --%>
	<br style="clear: both;" />
	<br />
	<br />

	<c:if test="${not empty case.listOfParticipants}">
		<display:table name="participants" class="table" id="participantList">
			<display:column property="lastName" media="html" titleKey="user.lastName" />
			<display:column property="firstName" media="html" titleKey="user.firstName" />
			<display:caption><fmt:message key="case.participantListCaption" /></display:caption>
		</display:table>
	</c:if>

	<input type="button" class="button" style="margin-right: 5px"
		onclick="location.href='<c:url value="userSearch?case=${case.caseId}"/>'"
		value="<fmt:message key="button.addParticipant"/>" />
</c:if>


