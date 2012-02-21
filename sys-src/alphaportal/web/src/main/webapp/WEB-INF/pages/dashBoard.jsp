<%@ page language="java" pageEncoding="utf-8" contentType="text/html;charset=utf-8"%>
<%@ include file="/common/taglibs.jsp"%>

<head>
    <title><fmt:message key="dashboard.title"/></title>
    
    <style type="text/css">
		div.leftBox {
		  width: 43%;
		  max-width: 43%;
		  min-width: 40%;
		  padding-right: 2px;
		  text-align: left;
		  float: left;
		  vertical-align: top;
		}
		
		div.rightBox {
		  width: 49%;
		  max-width: 49%;
		  min-width: 49%;
		  padding: 15px 20px 15px 20px;
		  float: right;
		  vertical-align: top;
		  border: 1px solid #F8F8F8;
		  border-radius: 20px;
		  background-color: #FAFAFA;
		}
		
		tr.activeCardRow {
		   background-color: rgba(149, 255, 149, 0.7) !important;
		}
	</style>
</head>

<div>
	<form method="post" id="contributorRequest">
		<display:table name="requests" class="table" requestURI="" id="row">
			<display:column>
			    <input type="radio" name="sel" value="<c:out value="${row.contributorRequestID}"/>" />
			</display:column>	 	
	
			<display:column property="alphaCard.alphaCardIdentifier.cardId" media="html" titleKey="card.id"/>
			<display:column property="alphaCard.alphaCardIdentifier.caseId" media="html" titleKey="case.id"/>
			<display:caption><fmt:message key="contributorrequest" /></display:caption>
		</display:table>
		
		<div class="buttonBar bottom leftbox" style="clear: left;">	
				<input type="submit" class="button right" name="denyRequest" value="<fmt:message key='button.deny'/>" />
	   	   	   	<input type="submit" class="button right" name="acceptRequest" value="<fmt:message key='button.accept'/>" />

	   	   	</div>  
	</form>
</div>

<br style="clear: left;"/>

<meta name="heading" content="<fmt:message key='dashboard.title'/>"/>
<div class="leftBox">
	<div id="caseTitleForm" class="toggableBox" style="display: none;">
       	<appfuse:label styleClass="desc" key="case.name"/>
	 	<c:out value="${case.name}" /> 
   	</div>
	<display:table name="cards" class="table" id="cards" style="width: 99%; margin-top: 10px;">
		<display:column property="alphaCardIdentifier.caseId" href="dashBoard?case=${cards.alphaCardIdentifier.caseId}&card=${cards.alphaCardIdentifier.cardId}" class="cardId"
			media="html" paramId="version" paramProperty="alphaCardIdentifier.sequenceNumber" titleKey="case.id" />
		<display:column property="alphaCardIdentifier.cardId" href="dashBoard?case=${cards.alphaCardIdentifier.caseId}&card=${cards.alphaCardIdentifier.cardId}" class="cardId"
			media="html" paramId="version" paramProperty="alphaCardIdentifier.sequenceNumber" titleKey="card.id" />
		<display:column property="alphaCardDescriptor.title" href="dashBoard?case=${cards.alphaCardIdentifier.caseId}&card=${cards.alphaCardIdentifier.cardId}"
			media="html" paramId="version" paramProperty="alphaCardIdentifier.sequenceNumber" titleKey="card.name" />
		<display:column property="alphaCardIdentifier.sequenceNumber" href="dashBoard?case=${cards.alphaCardIdentifier.caseId}&card=${cards.alphaCardIdentifier.cardId}"
			media="html" paramId="version" paramProperty="alphaCardIdentifier.sequenceNumber" titleKey="card.version" />
		<display:setProperty name="paging.banner.item_name">
			<fmt:message key="cardList.card" />
		</display:setProperty>
		<display:setProperty name="paging.banner.items_name">
			<fmt:message key="cardList.cards" />
		</display:setProperty>
		<display:caption><fmt:message key="dashboard.cardListCaption" /></display:caption>
	</display:table>
	<script type="text/javascript">
		Event.observe(window, 'load', function() {
	
			$("cards").select('tbody > tr').each(function(row) {
				if (row.select('a[href=]')[0].match(/version=${activeCard.alphaCardIdentifier.sequenceNumber}$/) > 0) {
					row.addClassName('activeCardRow');
				}
			});
		});
	</script>
</div>

<c:if test="${not empty activeCard}">
	<div class="rightBox">
		
		<h2>${activeCard.alphaCardDescriptor.title}</h2>
		
		<c:if test="${not empty activeCard.alphaCardIdentifier.cardId}">
   			<display:table name="activeCard.alphaCardDescriptor.allAdornments" class="table" id="adornments" style=" margin-top: 10px">
   				<c:if test="${not empty adornments.adornmentId}">
   					<display:column property="name" href="adornmentform?card=${activeCard.alphaCardIdentifier.cardId}&case=${activeCard.alphaCardIdentifier.caseId}" media="html" paramId="id" paramProperty="adornmentId" titleKey="adornment.name"/>
   					<display:column property="value" href="adornmentform?card=${activeCard.alphaCardIdentifier.cardId}&case=${activeCard.alphaCardIdentifier.caseId}" media="html" paramId="id" paramProperty="adornmentId" titleKey="adornment.value"/>
   				</c:if>
   				<display:setProperty name="paging.banner.item_name"><fmt:message key="card.adornmentList.adornment"/></display:setProperty>
   				<display:setProperty name="paging.banner.items_name"><fmt:message key="card.adornmentList.adornments"/></display:setProperty>
   				<display:caption><fmt:message key="card.adornmentListCaption" /></display:caption>
			</display:table>
		</c:if>
		<div class="buttonBar bottom">
			<input type="button" class="button right" value="<fmt:message key='button.cancel'/>" onclick="location.href='cardVersionHistory?case=${case.caseId}&card=${activeCard.alphaCardIdentifier.cardId}'" />

			<c:if test="${hidePayload == false}">
				<c:if test="${not empty activeCard.payload}">
					<input type="button" class="button" name="payloadGet" value="<fmt:message key="button.payload"/>" />
				</c:if>
			</c:if>
		</div>
	</div>
</c:if>



<div class="buttonBar bottom leftbox" style="clear: left;">	
	<input type="button" class="button right" onclick="location.href='caseform?caseId=${case.caseId}&activeCardId=<%= request.getParameter("card") %>'" value="<fmt:message key='button.cancel'/>" onclick="bCancel=true" />
</div>