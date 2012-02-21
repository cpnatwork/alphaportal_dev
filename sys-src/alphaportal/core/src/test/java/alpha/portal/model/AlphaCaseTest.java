package alpha.portal.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.appfuse.model.User;
import org.junit.Before;
import org.junit.Test;

public class AlphaCaseTest {

    AlphaCase alphaCase = new AlphaCase();

    private String testId = "4711";
    private String name = "testCase";

    AlphaCard alphaCard1 = new AlphaCard();
    AlphaCard alphaCard2 = new AlphaCard();
    AlphaCard alphaCard3 = new AlphaCard();

    private AlphaCardIdentifier testidentifier1 = new AlphaCardIdentifier(testId, "4811");
    private AlphaCardIdentifier testidentifier2 = new AlphaCardIdentifier(testId, "4812");
    private AlphaCardIdentifier testidentifier3 = new AlphaCardIdentifier(testId, "4813");

    User participant = new User();

    @Before
    public void setUp() {
        alphaCase.setCaseId(testId);
        alphaCase.setName(name);
        alphaCard1.setAlphaCardIdentifier(testidentifier1);
        alphaCard2.setAlphaCardIdentifier(testidentifier2);
        alphaCard3.setAlphaCardIdentifier(testidentifier3);
        participant.setId(5L);
    }

    @Test
    public void testGetCaseId() {
        assertTrue(alphaCase.getCaseId().equals(testId));
    }

    @Test
    public void testSetCaseId() {
        alphaCase.setCaseId("4911");
        assertTrue(alphaCase.getCaseId() == "4911");
    }

    @Test
    public void testGetName() {
        assertTrue(alphaCase.getName().equals(name));
    }

    @Test
    public void testSetName() {
        alphaCase.setName("Hugo");
        assertTrue(alphaCase.getName() == "Hugo");
    }

    @Test
    public void testParticipants() {
        assertEquals(0, alphaCase.getListOfParticipants().size());

        alphaCase.addParticipant(participant);
        assertEquals(1, alphaCase.getListOfParticipants().size());

        alphaCase.getListOfParticipants().contains(participant);

        alphaCase.removeParticipant(participant);
        assertEquals(0, alphaCase.getListOfParticipants().size());

        assertTrue(alphaCase.getParticipantsCRA() != null);
    }

    @Test
    public void testAlphaCard() {
        assertEquals(0, alphaCase.getAlphaCards().size());

        alphaCase.addAlphaCard(alphaCard1);
        assertEquals(1, alphaCase.getAlphaCards().size());

        alphaCase.getAlphaCards().contains(alphaCard1);
    }

    @Test
    public void testAddAlphaCard() {

        alphaCase.addAlphaCard(alphaCard1);

        alphaCase.addAlphaCard(alphaCard2);

        alphaCase.addAlphaCard(alphaCard3, 0);

        assertTrue(alphaCase.getAlphaCards().get(0).equals(alphaCard3));
        assertTrue(alphaCase.getAlphaCards().get(1).equals(alphaCard1));
        assertTrue(alphaCase.getAlphaCards().get(2).equals(alphaCard2));

    }

    @Test
    public void testMoveAlphaCard() {

        alphaCase.addAlphaCard(alphaCard1);

        alphaCase.addAlphaCard(alphaCard2);

        alphaCase.addAlphaCard(alphaCard3);

        alphaCase.moveAlphaCard(alphaCard1, 1);

        assertTrue(alphaCase.getAlphaCards().get(1).equals(alphaCard1));
        assertTrue(alphaCase.getAlphaCards().get(0).equals(alphaCard2));
        assertTrue(alphaCase.getAlphaCards().get(2).equals(alphaCard3));

    }

    @Test
    public void testRemoveAlphaCard() {

        alphaCase.addAlphaCard(alphaCard1);

        assertTrue(alphaCase.getAlphaCards().get(0).equals(alphaCard1));
        assertFalse(alphaCase.getAlphaCards().isEmpty());

        alphaCase.removeAlphaCard(alphaCard1);

        assertTrue(alphaCase.getAlphaCards().isEmpty());

        assertTrue(alphaCase.getAlphaCasePSA() != null);
    }

    @Test
    public void testEqualsObject() {
        AlphaCase alphaCase2 = new AlphaCase();
        alphaCase2.setCaseId(testId);
        alphaCase2.setName(name);

        assertTrue(alphaCase.equals(alphaCase2));

        assertFalse(alphaCase.equals(new AlphaCard()));
    }

    @Test
    public void testToString() {
        alphaCase.toString();
    }
}
