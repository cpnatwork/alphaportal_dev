package alpha.portal.model;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class PayloadIdentifierTest {

    @Test
    public void testEquals() {
        final Long payloadId = 1L;
        final Long seqNr = 1L;

        PayloadIdentifier id = new PayloadIdentifier();
        id.setPayloadId(payloadId);
        id.setSequenceNumber(seqNr);

        assertFalse(id.equals(new AlphaCard()));

        assertFalse(id.equals(new PayloadIdentifier()));

    }

    @Test
    public void testBasics() {
        final Long payloadId = 1L;
        final Long seqNr = 1L;

        PayloadIdentifier id = new PayloadIdentifier();
        id.setPayloadId(payloadId);
        assertTrue(payloadId == id.getPayloadId());
        id.setSequenceNumber(seqNr);
        assertTrue(seqNr == id.getSequenceNumber());
        int hash = id.hashCode();

    }
}
