<%@ page language="java" pageEncoding="utf-8" contentType="text/html;charset=utf-8"%>
<%@ include file="/common/taglibs.jsp"%>

<head>
    <title><fmt:message key="adornment.name"/></title>
    <meta name="heading" content="<fmt:message key='adornment.heading'/>"/>
</head>

<form:form commandName="adornment" method="post" action="adornmentform" id="adornmentForm" onsubmit="return validateAdornment(this)" acceptCharset="ISO-8859-1">
<form:errors path="*" cssClass="error" element="div"/>
<c:if test="${not empty adornment.adornmentId}">
	<form:hidden path="adornmentId"/>
</c:if>
<input type="hidden" name="case" value="<c:out value="${param.case}"/>"/>
<input type="hidden" name="card" value="<c:out value="${param.card}"/>"/>
<ul>
    <li>
        <appfuse:label styleClass="desc" key="adornment.name"/>
        <form:errors path="name" cssClass="fieldError"/>
        <form:input path="name" id="name" cssClass="text medium" cssErrorClass="text medium error" maxlength="50"/>
    </li>
    
    <li>
        <appfuse:label styleClass="desc" key="adornment.value"/>
        <form:errors path="value" cssClass="fieldError"/>
        
        <c:if test="${adornmentType == null}">
        	<form:input path="value" id="value" cssClass="text medium" cssErrorClass="text medium error" maxlength="50"/>
        </c:if>        
        <c:if test="${adornmentType != null && (adornmentType.valueType == 'String' || adornmentType.valueType == 'Integer' || adornmentType.valueType == 'Float')}">
        	<form:input path="value" id="value" cssClass="text medium" cssErrorClass="text medium error" maxlength="50"/>
        </c:if>
        <c:if test="${adornmentType != null && adornmentType.valueType == 'Enum' && adornmentType.valueRange != null}">
        	<form:select path="value" id="value" cssErrorClass="error" items="${adornmentType.valueRange.validStrings}" />
        </c:if>
    </li>

    <li class="buttonBar bottom">
       	<input type="submit" class="button" name="save" value="<fmt:message key="button.save"/>"/>
        <c:if test="${not empty adornment.adornmentId}">
        <input type="submit" class="button" name="delete" onclick="bCancel=true;return confirmDelete('adornment')"
            value="<fmt:message key="button.delete"/>" />
        </c:if>
        <input type="submit" class="button" name="cancel" value="<fmt:message key="button.cancel"/>" onclick="bCancel=true"/>
    </li>
    
   
</ul>
</form:form>

<script type="text/javascript">
    Form.focusFirstElement($('adornmentForm'));
</script>

<v:javascript formName="adornment" cdata="false" dynamicJavascript="true" staticJavascript="false"/>
<script type="text/javascript" src="<c:url value='/scripts/validator.jsp'/>"></script>