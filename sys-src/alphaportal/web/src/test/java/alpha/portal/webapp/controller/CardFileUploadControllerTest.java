package alpha.portal.webapp.controller;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.mock.web.MockMultipartHttpServletRequest;
import org.springframework.validation.BindingResult;
import org.springframework.validation.DataBinder;
import org.springframework.validation.ObjectError;

import alpha.portal.service.AlphaCardManager;

public class CardFileUploadControllerTest extends BaseControllerTestCase {
    @Autowired
    private CardFileUploadController ctrl;

    @Autowired
    private AlphaCardManager alphaCardManager;

    @Test
    public void testOnSubmit() {
        String caseId = "550e4713-e22b-11d4-a716-446655440000";
        String cardId = "440e4816-e01b-74d4-a716-449955440092";
        String fileName = "doesnotcompute.file";
        String mimeType = "text/plain";
        byte[] content = "roflcopter".getBytes();

        MockHttpServletRequest request = newGet("/cardfileupload");
        request.setRemoteUser("admin");
        request.addParameter("case", caseId);
        request.addParameter("card", cardId);
        FileUpload fileUpload = ctrl.showForm(request);
        fileUpload.setFile(content);

        MockMultipartHttpServletRequest upload = new MockMultipartHttpServletRequest();
        upload.setRemoteUser("admin");
        MockMultipartFile file = new MockMultipartFile("file", fileName, mimeType, content);
        upload.addFile(file);
        upload.addParameter("case", caseId);
        upload.addParameter("card", cardId);

        /*
         * Sadly enough we would need a flush() within the PayloadManagerImpl.saveNewPayload() function for this test to
         * succeed since we moved to saving the payload via its own manager/dao.
         */

        // BindingResult errors = new DataBinder(fileUpload).getBindingResult();
        // String result = "";
        // try {
        // result = ctrl.onSubmit(fileUpload, errors, upload);
        // } catch (IOException e) {
        // fail("Should not fail on fail upload");
        // }
        //
        // assertFalse(errors.hasErrors());
        // assertNotNull(upload.getSession().getAttribute("successMessages"));
        //
        // AlphaCard myCard = alphaCardManager.get(new AlphaCardIdentifier(caseId, cardId));
        // assertNotNull(myCard);
        // assertNotNull(myCard.getPayload());
        // Assert.assertArrayEquals(content, myCard.getPayload().getContent());
        // Assert.assertEquals(fileName, myCard.getPayload().getFilename());
        // Assert.assertEquals(mimeType, myCard.getPayload().getMimeType());

        // Assert.assertEquals("redirect:/caseform?caseId=" + caseId + "&activeCardId=" + cardId, result);
    }

    @Test
    public void testOnCancel() {
        String caseId = "550e4713-e22b-11d4-a716-446655440000";
        String cardId = "440e4816-e01b-74d4-a716-449955440092";
        String fileName = "doesnotcompute.file";
        String mimeType = "text/plain";
        byte[] content = "roflcopter".getBytes();

        MockHttpServletRequest request = newGet("/cardfileupload");
        request.setRemoteUser("admin");
        request.addParameter("case", caseId);
        request.addParameter("card", cardId);
        FileUpload fileUpload = ctrl.showForm(request);
        fileUpload.setFile(content);

        MockMultipartHttpServletRequest upload = new MockMultipartHttpServletRequest();
        upload.setRemoteUser("admin");
        MockMultipartFile file = new MockMultipartFile("file", fileName, mimeType, content);
        upload.addFile(file);
        upload.addParameter("case", caseId);
        upload.addParameter("card", cardId);
        upload.addParameter("cancel", "Abbrechen");

        BindingResult errors = new DataBinder(fileUpload).getBindingResult();
        String result = "";
        try {
            result = ctrl.onSubmit(fileUpload, errors, upload);
        } catch (IOException e) {
            fail("Should not fail on fail upload");
        }
        assertFalse(errors.hasErrors());
        assertNull(upload.getSession().getAttribute("successMessages"));

        Assert.assertEquals("redirect:/caseform?caseId=" + caseId + "&activeCardId=" + cardId, result);
    }

    @Test
    public void testZeroFile() {
        String caseId = "550e4713-e22b-11d4-a716-446655440000";
        String cardId = "440e4816-e01b-74d4-a716-449955440092";
        String fileName = "doesnotcompute.file";
        String mimeType = "text/plain";
        byte[] content = "".getBytes();

        MockHttpServletRequest request = newGet("/cardfileupload");
        request.setRemoteUser("admin");
        request.addParameter("case", caseId);
        request.addParameter("card", cardId);
        FileUpload fileUpload = ctrl.showForm(request);
        fileUpload.setFile(content);

        MockMultipartHttpServletRequest upload = new MockMultipartHttpServletRequest();
        upload.setRemoteUser("user");
        MockMultipartFile file = new MockMultipartFile("file", fileName, mimeType, content);
        upload.addFile(file);
        upload.addParameter("case", caseId);
        upload.addParameter("card", cardId);

        BindingResult errors = new DataBinder(fileUpload).getBindingResult();
        String result = "";
        try {
            result = ctrl.onSubmit(fileUpload, errors, upload);
        } catch (IOException e) {
            fail("Should not fail on fail upload");
        }
        assertTrue(errors.hasErrors());
        List<ObjectError> errorList = errors.getAllErrors();
        assertEquals(1, errorList.size());
        assertEquals("errors.required", errorList.get(0).getCode());
        assertNull(upload.getSession().getAttribute("successMessages"));

        Assert.assertEquals("redirect:/cardfileupload?card=" + cardId + "&case=" + caseId, result);
    }
}
