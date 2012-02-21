package alpha.portal.webapp.controller;

import java.io.IOException;
import java.util.Locale;

import javax.servlet.http.HttpServletRequest;

import org.appfuse.model.User;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.multipart.MultipartHttpServletRequest;

import alpha.portal.model.Adornment;
import alpha.portal.model.AdornmentType;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardIdentifier;
import alpha.portal.model.Payload;
import alpha.portal.service.AlphaCardManager;
import alpha.portal.service.PayloadManager;

/**
 * Controller class to upload Files.
 * <p/>
 * <p>
 * <a href="FileUploadFormController.java.html"><i>View Source</i></a>
 * </p>
 * 
 * @author <a href="mailto:matt@raibledesigns.com">Matt Raible</a>
 */

@Controller
@RequestMapping("/cardfileupload*")
public class CardFileUploadController extends BaseFormController {

    /**
     * the AlphaCardManager.
     * 
     * @see AlphaCardManager AlphaCardManager
     */
    @Autowired
    AlphaCardManager alphaCardManager;

    @Autowired
    PayloadManager payloadManager;

    /**
     * default constructor
     */
    public CardFileUploadController() {

    }

    /**
     * shows the card file upload site.
     * 
     * @param request
     * 
     * @return FileUpload
     * 
     * @see cardfileupload.jsp
     */
    @ModelAttribute
    @RequestMapping(method = RequestMethod.GET)
    public FileUpload showForm(final HttpServletRequest request) {

        String caseId = request.getParameter("case");
        String cardId = request.getParameter("card");
        Locale locale = request.getLocale();

        request.setAttribute("case", caseId);
        request.setAttribute("card", cardId);

        setCancelView("redirect:/caseform?caseId=" + caseId + "&activeCardId=" + cardId);
        setSuccessView("redirect:/caseform?caseId=" + caseId + "&activeCardId=" + cardId);

        return new FileUpload();
    }

    /**
     * handles the case, if the user clicks on one of the buttons.
     * 
     * @param fileUpload
     * 
     * @param errors
     * 
     * @param request
     * 
     * @return success view
     * 
     * @throws IOException
     */
    @RequestMapping(method = RequestMethod.POST)
    public String onSubmit(final FileUpload fileUpload, final BindingResult errors, final HttpServletRequest request)
            throws IOException {

        String caseId = request.getParameter("case");
        String cardId = request.getParameter("card");
        Locale locale = request.getLocale();

        setCancelView("redirect:/caseform?caseId=" + caseId + "&activeCardId=" + cardId);
        setSuccessView("redirect:/caseform?caseId=" + caseId + "&activeCardId=" + cardId);

        AlphaCard card = alphaCardManager.get(new AlphaCardIdentifier(caseId, cardId));
        if (card == null) {
            saveError(request, getText("card.invalidId", locale));
            return getCancelView();
        }
        Adornment contributor = card.getAlphaCardDescriptor().getAdornment(AdornmentType.Contributor.getName());

        if (contributor.getValue() == null || contributor.getValue().isEmpty()) {

            saveError(request, getText("adornment.noAccess", locale));
            return getCancelView();

        } else {

            Long contributorID = Long.parseLong(contributor.getValue());
            User currentUser = getUserManager().getUserByUsername(request.getRemoteUser());

            if (contributorID != currentUser.getId()) {

                saveError(request, getText("adornment.noAccess", locale));
                return getCancelView();
            }
        }

        if (request.getParameter("cancel") != null)
            return getCancelView();

        if (validator != null) { // validator is null during testing
            fileUpload.setName("alphaCardPayloadFile");

            validator.validate(fileUpload, errors);

            if (errors.hasErrors())
                return "redirect:/cardfileupload?card=" + cardId + "&case=" + caseId;
        }

        // validate a file was entered
        if (fileUpload.getFile().length == 0) {
            Object[] args = new Object[] { getText("uploadForm.file", request.getLocale()) };
            errors.rejectValue("file", "errors.required", args, "File");

            return "redirect:/cardfileupload?card=" + cardId + "&case=" + caseId;
        }

        MultipartHttpServletRequest multipartRequest = (MultipartHttpServletRequest) request;
        MultipartFile file = multipartRequest.getFile("file");

        Payload payload = new Payload(file.getOriginalFilename(), file.getContentType());
        payload.setContent(file.getBytes());

        payload = payloadManager.saveNewPayload(payload, card);

        saveMessage(request, getText("card.payloadOK", locale));
        return getSuccessView();
    }
}
