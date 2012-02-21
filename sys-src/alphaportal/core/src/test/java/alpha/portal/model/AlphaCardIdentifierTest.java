package alpha.portal.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import org.junit.Test;

public class AlphaCardIdentifierTest {
    @Test
    public void testBasics() {
        final String cardId = "lol";
        final String caseId = "rofl";

        AlphaCardIdentifier id = new AlphaCardIdentifier();
        id.setCardId(cardId);
        assertEquals(cardId, id.getCardId());
        id.setCaseId(caseId);
        assertEquals(caseId, id.getCaseId());
        int hash = id.hashCode();

        AlphaCardIdentifier id2 = new AlphaCardIdentifier(caseId);
        id2.setCardId(cardId);
        assertEquals(id, id2);
        assertEquals(hash, id2.hashCode());

        AlphaCardIdentifier id3 = new AlphaCardIdentifier(caseId, cardId);
        assertEquals(id, id3);
        assertEquals(hash, id3.hashCode());
    }

    @Test
    public void testEquals() {
        final String cardId = "lol";
        final String caseId = "123";
        final Long sequenceNumber = 1L;

        AlphaCardIdentifier id = new AlphaCardIdentifier();
        id.setCardId(cardId);
        id.setCaseId(caseId);
        id.setSequenceNumber(sequenceNumber);

        assertFalse(id.equals(new AlphaCard()));

        AlphaCardIdentifier id2 = id.clone();
        assertEquals(id, id2);
    }
}
