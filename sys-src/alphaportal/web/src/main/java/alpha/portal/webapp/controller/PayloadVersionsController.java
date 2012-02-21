package alpha.portal.webapp.controller;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.List;
import java.util.Locale;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.appfuse.model.User;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.util.FileCopyUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.ModelAndView;

import alpha.portal.model.AdornmentType;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardIdentifier;
import alpha.portal.model.Payload;
import alpha.portal.model.PayloadIdentifier;
import alpha.portal.service.AlphaCardManager;
import alpha.portal.service.PayloadManager;

/**
 * Shows all payload versions; only to contributor!
 * 
 */

@Controller
@RequestMapping("/payloadVersions*")
public class PayloadVersionsController extends BaseFormController {

    /**
     * the PayloadManager
     * 
     * @see PayloadManager PayloadManager
     */
    @Autowired
    private PayloadManager payloadManager;

    /**
     * the AlphaCardManager
     * 
     * @see AlphaCardManager AlphaCardManager
     */
    @Autowired
    private AlphaCardManager alphaCardManager;

    /**
     * shows the list of payload versions
     * 
     * @param request
     * 
     * @return ModelView
     * 
     * @throws Exception
     */
    @RequestMapping(method = RequestMethod.GET)
    public ModelAndView handleRequest(HttpServletRequest request)
            throws Exception {

        ModelAndView returnMaV = new ModelAndView();

        User currentUser = getUserManager().getUserByUsername(
                request.getRemoteUser());
        Locale locale = request.getLocale();

        if (request.getParameter("card") == null
                || request.getParameter("case") == null) {
            saveError(request, getText("payloadVersions.cardNotFound", locale));
            returnMaV.addObject("isErrors", true);
            return returnMaV;
        }
        String cardId = request.getParameter("card");
        String caseId = request.getParameter("case");

        AlphaCard currentCard = alphaCardManager.get(new AlphaCardIdentifier(
                caseId, cardId));
        if (currentCard == null) {
            saveError(request, getText("payloadVersions.cardNotFound", locale));
            returnMaV.addObject("isErrors", true);
            return returnMaV;
        }

        Long cardContributor = null;
        try {
            cardContributor = Long.parseLong(currentCard
                    .getAlphaCardDescriptor().getAdornment(
                            AdornmentType.Contributor.getName()).getValue());
        } catch (NumberFormatException e) {
            cardContributor = 0L;
        }
        Long currentUserId = currentUser.getId();
        if (cardContributor != currentUserId) {
            saveError(request, getText("adornment.noAccess", locale));
            returnMaV.addObject("isErrors", true);
            return returnMaV;
        }

        List<Payload> payloads = payloadManager.getAllVersions(currentCard
                .getPayload());
        if (payloads.size() < 1) {
            saveError(request, getText("payloadVersions.noPayloads", locale));
            returnMaV.addObject("isErrors", true);
            return returnMaV;
        }

        returnMaV.addObject("payloadList", payloads);
        returnMaV.addObject("cardName", currentCard.getAlphaCardDescriptor()
                .getTitle());
        returnMaV.addObject("caseId", currentCard.getAlphaCardIdentifier()
                .getCaseId());
        returnMaV.addObject("cardId", currentCard.getAlphaCardIdentifier()
                .getCardId());

        return returnMaV;
    }

    @RequestMapping(method = RequestMethod.GET, params = { "seqNumber" })
    public String getPayload(final HttpServletRequest request,
            final HttpServletResponse response) throws IOException {
        Locale locale = request.getLocale();
        if (request.getParameter("card") == null
                || request.getParameter("case") == null) {
            saveError(request, getText("payloadVersions.cardNotFound", locale));
            return "";
        }
        String cardId = request.getParameter("card");
        String caseId = request.getParameter("case");

        AlphaCard currentCard = alphaCardManager.get(new AlphaCardIdentifier(
                caseId, cardId));
        final Payload payload = payloadManager
                .getVersion(new PayloadIdentifier(currentCard.getPayload()
                        .getPayloadIdentifier().getPayloadId(), Long
                        .parseLong(request.getParameter("seqNumber"))));

        if (payload != null) {

            final BufferedInputStream in = new BufferedInputStream(
                    new ByteArrayInputStream(payload.getContent()));

            response.setBufferSize(payload.getContent().length);
            response.setContentType(payload.getMimeType());
            response.setHeader("Content-Disposition", "attachment; filename=\""
                    + payload.getFilename() + "\"");
            response.setContentLength(payload.getContent().length);

            FileCopyUtils.copy(in, response.getOutputStream());
            in.close();
            response.getOutputStream().flush();
            response.getOutputStream().close();
        }

        return "redirect:/payloadVersions?case="
                + currentCard.getAlphaCardIdentifier().getCaseId() + "&card="
                + currentCard.getAlphaCardIdentifier().getCardId();
    }
}
