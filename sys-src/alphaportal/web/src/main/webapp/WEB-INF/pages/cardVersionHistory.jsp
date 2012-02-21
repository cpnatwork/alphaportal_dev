<%@ page language="java" pageEncoding="utf-8" contentType="text/html;charset=utf-8"%>
<%@ include file="/common/taglibs.jsp"%>

<head>
    <title><fmt:message key="card.versionHistory"/></title>
    
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
<c:if test="${!noAccess}">
	<meta name="heading" content="<fmt:message key='card.versionHistory'/>"/>
	<div class="leftBox">
		<div id="caseTitleForm" class="toggableBox" style="display: none;">
	       	<appfuse:label styleClass="desc" key="case.name"/>
   		 	<c:out value="${case.name}" /> 
   	   	</div>
		<display:table name="cards" class="table" id="cards" style="width: 99%; margin-top: 10px;" pagesize="20" excludedParams="*" requestURI="cardVersionHistory?case=${case.caseId}" >
			<display:column property="alphaCardIdentifier.cardId" href="cardVersionHistory?case=${case.caseId}&card=${cards.alphaCardIdentifier.cardId}&${paging}" class="cardId"
				media="html" paramId="version" paramProperty="alphaCardIdentifier.sequenceNumber" titleKey="card.id" />
			<display:column property="alphaCardDescriptor.title" href="cardVersionHistory?case=${case.caseId}&card=${cards.alphaCardIdentifier.cardId}&${paging}"
				media="html" paramId="version" paramProperty="alphaCardIdentifier.sequenceNumber" titleKey="card.name" />
			<display:column property="alphaCardIdentifier.sequenceNumber" href="cardVersionHistory?case=${case.caseId}&card=${cards.alphaCardIdentifier.cardId}&${paging}"
				media="html" paramId="version" paramProperty="alphaCardIdentifier.sequenceNumber" titleKey="card.version" />
			<display:setProperty name="paging.banner.item_name">
				<fmt:message key="cardList.version" />
			</display:setProperty>
			<display:setProperty name="paging.banner.items_name">
				<fmt:message key="cardList.versions" />
			</display:setProperty>
			<display:caption><fmt:message key="case.cardListCaption" /></display:caption>
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
	   					<display:column property="name" paramId="id" paramProperty="adornmentId" titleKey="adornment.name"/>
	   					<display:column property="value" media="html" paramId="id" paramProperty="adornmentId" titleKey="adornment.value"/>
	   				</c:if>
	   				<display:setProperty name="paging.banner.item_name"><fmt:message key="card.adornmentList.adornment"/></display:setProperty>
	   				<display:setProperty name="paging.banner.items_name"><fmt:message key="card.adornmentList.adornments"/></display:setProperty>
	   				<display:caption><fmt:message key="card.adornmentListCaption" /></display:caption>
				</display:table>
			</c:if>
			<div class="buttonBar bottom">
				<input type="button" class="button right" value="<fmt:message key='button.cancel'/>" onclick="location.href='cardVersionHistory?case=${case.caseId}&card=${activeCard.alphaCardIdentifier.cardId}&${paging}'" />
			</div>
		</div>
	</c:if>
</c:if>

<div class="buttonBar bottom leftbox" style="clear: left;">	
	<input type="button" class="button right" onclick="location.href='caseform?caseId=${case.caseId}&activeCardId=<%= request.getParameter("card") %>'" value="<fmt:message key='button.cancel'/>" onclick="bCancel=true" />
</div>