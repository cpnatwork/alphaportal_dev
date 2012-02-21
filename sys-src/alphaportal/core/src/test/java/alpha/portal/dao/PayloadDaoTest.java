package alpha.portal.dao;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.appfuse.dao.BaseDaoTestCase;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCase;
import alpha.portal.model.Payload;
import alpha.portal.model.PayloadIdentifier;
import alpha.portal.service.AlphaCardManager;
import alpha.portal.service.impl.CaseManagerImpl;

public class PayloadDaoTest extends BaseDaoTestCase {

    /** The payload dao. */
    @Autowired
    private PayloadDao payloadDao;

    @Autowired
    private AlphaCardManager alphaCardManager;

    @Autowired
    private CaseManagerImpl caseManager;

    @Test
    public void testGetAllVersions() {
        AlphaCase aCase = new AlphaCase();
        aCase = caseManager.save(aCase);

        // AlphaCard aCard = alphaCardManager.createAlphaCard(aCase.getCaseId());
        AlphaCard aCard = new AlphaCard(aCase);

        byte[] content = "payload data1".getBytes();
        Payload payload = new Payload();
        payload.setFilename("filename");
        payload.setMimeType("text/plain");
        payload.setContent(content);
        payload = payloadDao.save(payload); // first payload version
        flush();

        List<Payload> payloads = payloadDao.getAllVersions(payload);
        assertEquals(1, payloads.size());

        aCard.setPayload(payload);
        aCard = alphaCardManager.save(aCard);
        flush();

        aCard.setPayload(null);
        aCard = alphaCardManager.save(aCard);

        payloads = payloadDao.getAllVersions(payload);
        assertEquals(1, payloads.size());

        content = "payload data2".getBytes();
        Payload payload2 = new Payload();
        payload2.setPayloadIdentifier(new PayloadIdentifier(payload
                .getPayloadIdentifier().getPayloadId(), 0));
        payload2.setFilename("filename");
        payload2.setMimeType("text/plain");
        payload2.setContent(content);
        payload2 = payloadDao.save(payload2); // second payload version
        flush();

        aCard.setPayload(payload2);
        aCard = alphaCardManager.save(aCard);

        payloads = payloadDao.getAllVersions(payload2);
        assertEquals(2, payloads.size());

    }

    @Test
    public void testGetVersion() {

        AlphaCase aCase = new AlphaCase();

        aCase = caseManager.save(aCase);

        //AlphaCard aCard = alphaCardManager.createAlphaCard(aCase.getCaseId());
        AlphaCard aCard = new AlphaCard(aCase);

        byte[] content = "payload data1".getBytes();
        Payload payload = new Payload();
        payload.setFilename("filename");
        payload.setMimeType("text/plain");
        payload.setContent(content);
        payload = payloadDao.save(payload);
        flush();

        aCard.setPayload(payload);

        aCard = alphaCardManager.save(aCard);
        flush();
        aCard.setPayload(null);
        aCard = alphaCardManager.save(aCard);

        content = "payload data2".getBytes();
        Payload payload2 = new Payload();
        payload2.setFilename("filename");
        payload2.setMimeType("text/plain");
        payload2.setContent(content);
        payload2 = payloadDao.save(payload2);
        flush();

        aCard.setPayload(payload2);

        aCard = alphaCardManager.save(aCard);

        Payload backPayload = payloadDao.getVersion(payload2
                .getPayloadIdentifier());

        assertTrue(backPayload.getPayloadIdentifier().getSequenceNumber() == payload2
                .getPayloadIdentifier().getSequenceNumber());

    }

    @Test
    public void testSave() {

        AlphaCase aCase = new AlphaCase();

        aCase = caseManager.save(aCase);

        //AlphaCard aCard = alphaCardManager.createAlphaCard(aCase.getCaseId());
        AlphaCard aCard = new AlphaCard(aCase);

        byte[] content = "payload data1".getBytes();
        Payload payload = new Payload();
        payload.setFilename("filename");
        payload.setMimeType("text/plain");
        payload.setContent(content);
        payload = payloadDao.save(payload);
        flush();

        Long payloadId = payload.getPayloadIdentifier().getPayloadId();

        aCard.setPayload(payload);

        aCard = alphaCardManager.save(aCard);
        flush();

        assertTrue(payloadId == payloadDao.getVersion(
                payload.getPayloadIdentifier()).getPayloadIdentifier()
                .getPayloadId());

        int oldVersionCount = payloadDao.getAllVersions(payload).size();
        byte[] content2 = "payload data2".getBytes();
        payload.setContent(content2);
        payload = payloadDao.save(payload);
        flush();

        assertFalse(oldVersionCount == payloadDao.getAllVersions(payload)
                .size());
    }
}
