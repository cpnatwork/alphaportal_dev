package alpha.portal.webapp.controller;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Enumeration;
import java.util.Locale;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.appfuse.model.User;
import org.appfuse.service.UserManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.util.FileCopyUtils;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import alpha.portal.model.Adornment;
import alpha.portal.model.AdornmentType;
import alpha.portal.model.AdornmentTypeDeleted;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardIdentifier;
import alpha.portal.model.AlphaCase;
import alpha.portal.model.Payload;
import alpha.portal.service.AlphaCardManager;
import alpha.portal.service.CaseManager;

/**
 * Controller of the card form.
 * 
 * @see BaseFormController Base FormController
 */
@Controller
@RequestMapping("/cardform*")
public class CardFormController extends BaseFormController {

    /**
     * the AlphaCardManager.
     * 
     * @see AlphaCardManager AlphaCardManager
     */
    @Autowired
    private AlphaCardManager alphaCardManager;

    /**
     * the CaseManager for setting the correct priority when creating.
     * 
     * @see CaseManager
     */
    @Autowired
    private CaseManager caseManager;

    /**
     * the userManager.
     * 
     * @see UserManager UserManager
     */
    @Autowired
    private UserManager userManager;

    /**
     * shows the card form.
     * 
     * @param request
     * 
     * @return ModelView
     * 
     * @see cardform.jsp
     */
    @SuppressWarnings("unchecked")
    @ModelAttribute("alphacard")
    @RequestMapping(method = RequestMethod.GET)
    protected String showForm(final HttpServletRequest request) {

        log.fatal("This fallback Method should not be called");

        final Enumeration params = request.getParameterNames();
        while (params.hasMoreElements()) {
            log.error(params.nextElement().toString());
        }

        return "redirect:/caseMenu";
    }

    /**
     * handles the case, if the user clicks on one of the buttons.
     * 
     * @param myCard
     * 
     * @param errors
     * 
     * @param request
     * 
     * @param response
     * 
     * @return success view
     * 
     * @throws Exception
     */
    @SuppressWarnings("unchecked")
    @RequestMapping(method = RequestMethod.POST)
    public String onSubmit(final AlphaCard myCard, final BindingResult errors, final HttpServletRequest request,
            final HttpServletResponse response) throws Exception {

        log.fatal("This fallback Method should not be called");

        final Enumeration params = request.getParameterNames();
        while (params.hasMoreElements()) {
            log.error(params.nextElement().toString());
        }

        return "redirect:/caseform?caseId=" + myCard.getAlphaCardIdentifier().getCaseId();
    }

    @RequestMapping(method = RequestMethod.POST, params = { "saveCard" })
    public String saveCard(final AlphaCard jspCard, final BindingResult errors, final HttpServletRequest request,
            final HttpServletResponse response) throws Exception {
        AlphaCard alphaCard = null;
        AlphaCardIdentifier identifier = null;
        Locale locale = request.getLocale();

        if (!StringUtils.isBlank(jspCard.getAlphaCardIdentifier().getCardId())) {
            alphaCard = alphaCardManager.get(jspCard.getAlphaCardIdentifier());
            identifier = alphaCard.getAlphaCardIdentifier();

            String cardId = identifier.getCardId();
            String caseId = identifier.getCaseId();

            Adornment contributor = alphaCard.getAlphaCardDescriptor()
                    .getAdornment(AdornmentType.Contributor.getName());

            if (contributor.getValue() == null || contributor.getValue().isEmpty()) {

                saveError(request, getText("adornment.noAccess", locale));
                return "redirect:/caseform?activeCardId=" + cardId + "&caseId=" + caseId;

            } else {

                Long contributorID = Long.parseLong(contributor.getValue());
                User currentUser = getUserManager().getUserByUsername(request.getRemoteUser());

                if (contributorID != currentUser.getId()) {
                    saveError(request, getText("adornment.noAccess", locale));
                    return "redirect:/caseform?activeCardId=" + cardId + "&caseId=" + caseId;
                }
            }

            /**
             * Check if card has been deleted
             */
            Adornment delAdornment = alphaCard.getAlphaCardDescriptor().getAdornment(AdornmentType.Deleted.getName());
            if (delAdornment != null) {
                String delIsTrue = AdornmentTypeDeleted.TRUE.value();
                if (delAdornment.getValue().equals(delIsTrue)) {
                    saveError(request, getText("adornment.errorChange", locale));
                    return "redirect:/caseform?activeCardId=" + cardId + "&caseId=" + caseId;
                }
            }

            alphaCard.getAlphaCardDescriptor().setTitle(jspCard.getAlphaCardDescriptor().getTitle());

        } else {
            alphaCard = alphaCardManager.createAlphaCard(jspCard.getAlphaCardIdentifier().getCaseId());
            User currentUser = userManager.getUserByUsername(request.getRemoteUser());
            alphaCard.getAlphaCardDescriptor().setContributor(currentUser.getId());
            for (final Adornment a : jspCard.getAlphaCardDescriptor().getAllAdornments()) {
                alphaCard.getAlphaCardDescriptor().setAdornment(a.getName(), a.getValue());
            }
            identifier = alphaCard.getAlphaCardIdentifier();
            final AlphaCase alphaCase = caseManager.get(identifier.getCaseId());
            alphaCase.addAlphaCard(alphaCard);
        }

        alphaCard = alphaCardManager.save(alphaCard);

        saveMessage(request, getText("card.updated", locale));
        return "redirect:/caseform?caseId=" + identifier.getCaseId() + "&activeCardId=" + identifier.getCardId();
    }

    @RequestMapping(method = RequestMethod.POST, params = { "cancelCard" })
    public String cancelCard(final AlphaCard alphaCard, final BindingResult errors, final HttpServletRequest request)
            throws Exception {
        return "redirect:/caseform?caseId=" + alphaCard.getAlphaCardIdentifier().getCaseId();
    }

    @RequestMapping(method = RequestMethod.POST, params = { "assignToMe" })
    public String assignCard(final AlphaCard jspCard, final BindingResult errors, final HttpServletRequest request)
            throws Exception {
        final AlphaCard alphaCard = alphaCardManager.get(jspCard.getAlphaCardIdentifier());
        final User currentUser = userManager.getUserByUsername(request.getRemoteUser());
        alphaCard.getAlphaCardDescriptor().setAdornment(AdornmentType.Contributor.getName(),
                currentUser.getId().toString());

        alphaCardManager.save(alphaCard);

        final AlphaCardIdentifier identifier = alphaCard.getAlphaCardIdentifier();
        return "redirect:/caseform?caseId=" + identifier.getCaseId() + "&activeCardId=" + identifier.getCardId();
    }

    @RequestMapping(method = RequestMethod.POST, params = { "unassignMe" })
    public String unassignCard(final AlphaCard jspCard, final BindingResult errors, final HttpServletRequest request)
            throws Exception {

        final AlphaCard alphaCard = alphaCardManager.get(jspCard.getAlphaCardIdentifier());
        alphaCard.getAlphaCardDescriptor().setAdornment(AdornmentType.Contributor.getName(), "");

        alphaCardManager.save(alphaCard);

        final AlphaCardIdentifier identifier = alphaCard.getAlphaCardIdentifier();
        return "redirect:/caseform?caseId=" + identifier.getCaseId() + "&activeCardId=" + identifier.getCardId();
    }

    @RequestMapping(method = RequestMethod.POST, params = { "payloadGet" })
    public String getPayload(final AlphaCard jspCard, final HttpServletResponse response) throws IOException {
        final AlphaCard alphaCard = alphaCardManager.get(jspCard.getAlphaCardIdentifier());
        final Payload payload = alphaCard.getPayload();

        if (payload != null) {

            final BufferedInputStream in = new BufferedInputStream(new ByteArrayInputStream(payload.getContent()));

            response.setBufferSize(payload.getContent().length);
            response.setContentType(payload.getMimeType());
            response.setHeader("Content-Disposition", "attachment; filename=\"" + payload.getFilename() + "\"");
            response.setContentLength(payload.getContent().length);

            FileCopyUtils.copy(in, response.getOutputStream());
            in.close();
            response.getOutputStream().flush();
            response.getOutputStream().close();
        }

        final AlphaCardIdentifier identifier = alphaCard.getAlphaCardIdentifier();
        return "redirect:/caseform?caseId=" + identifier.getCaseId() + "&activeCardId=" + identifier.getCardId();
    }

    @RequestMapping(method = RequestMethod.POST, params = { "payloadDelete" })
    public String deletePayload(final AlphaCard jspCard, final HttpServletRequest request) throws IOException {
        final AlphaCard alphaCard = alphaCardManager.get(jspCard.getAlphaCardIdentifier());
        alphaCard.setPayload(null);
        alphaCardManager.save(alphaCard);

        final Locale locale = request.getLocale();
        saveMessage(request, getText("card.updated", locale));

        AlphaCardIdentifier identifier = alphaCard.getAlphaCardIdentifier();
        return "redirect:/caseform?caseId=" + identifier.getCaseId() + "&activeCardId=" + identifier.getCardId();
    }

    @RequestMapping(method = RequestMethod.POST, params = { "setDeleted" })
    public String setACardDeleted(final AlphaCard jspCard, final HttpServletRequest request) throws Exception {
        return setACardDeletedStatus(jspCard, request);
    }

    @RequestMapping(method = RequestMethod.POST, params = { "setNotDeleted" })
    public String setACardNotDeleted(final AlphaCard jspCard, final HttpServletRequest request) throws Exception {
        return setACardDeletedStatus(jspCard, request);
    }

    private String setACardDeletedStatus(final AlphaCard jspCard, final HttpServletRequest request) throws Exception {

        final AlphaCard alphaCard = alphaCardManager.get(jspCard.getAlphaCardIdentifier());

        Adornment contributor = alphaCard.getAlphaCardDescriptor().getAdornment(AdornmentType.Contributor.getName());
        if (contributor.getValue() == null || contributor.getValue().isEmpty()) {
            saveError(request, getText("adornment.noAccess", request.getLocale()));
            String cardId = alphaCard.getAlphaCardIdentifier().getCardId();
            String caseId = alphaCard.getAlphaCardIdentifier().getCaseId();
            return "redirect:/caseform?activeCardId=" + cardId + "&caseId=" + caseId;
        } else {
            Long contributorID = Long.parseLong(contributor.getValue());
            User currentUser = getUserManager().getUserByUsername(request.getRemoteUser());
            if (contributorID != currentUser.getId()) {
                saveError(request, getText("adornment.noAccess", request.getLocale()));
                String cardId = alphaCard.getAlphaCardIdentifier().getCardId();
                String caseId = alphaCard.getAlphaCardIdentifier().getCaseId();
                return "redirect:/caseform?activeCardId=" + cardId + "&caseId=" + caseId;
            }
        }

        Adornment deletedAdornment = null;
        if (alphaCard.getAlphaCardDescriptor().getAdornment(AdornmentType.Deleted.getName()) == null) {
            deletedAdornment = new Adornment(AdornmentType.Deleted.getName());
            deletedAdornment.setValue(AdornmentTypeDeleted.FALSE.value());
            alphaCard.getAlphaCardDescriptor().setAdornment(deletedAdornment);
        }

        deletedAdornment = alphaCard.getAlphaCardDescriptor().getAdornment(AdornmentType.Deleted.getName());

        if (request.getParameter("setDeleted") != null) {
            deletedAdornment.setValue(AdornmentTypeDeleted.TRUE.value());
            alphaCardManager.save(alphaCard);
            saveMessage(request, getText("card.deleted", request.getLocale()));
        } else if (request.getParameter("setNotDeleted") != null) {
            deletedAdornment.setValue(AdornmentTypeDeleted.FALSE.value());
            alphaCardManager.save(alphaCard);
            saveMessage(request, getText("card.updated", request.getLocale()));
        }

        AlphaCardIdentifier identifier = alphaCard.getAlphaCardIdentifier();
        return "redirect:/caseform?caseId=" + identifier.getCaseId() + "&activeCardId=" + identifier.getCardId();
    }
}
