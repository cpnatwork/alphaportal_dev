package alpha.client;

import java.io.IOException;

import org.junit.Test;

public class RESTClientTest {
    @Test
    public void testGetAdornment() throws IOException {
        String[] args = { "-get", "-csid", "550e4713-e22b-11d4-a716-446655440002", "-cdid",
                "440e4816-e01b-74d4-a716-449955440097", "-aName", "Verantwortlicher", "-hn", "localhost:8080" };
        RESTClient.main(args);
    }

    @Test
    public void testPostAdornment() throws IOException {
        String[] args = { "-add", "-csid", "550e4713-e22b-11d4-a716-446655440002", "-cdid",
                "440e4816-e01b-74d4-a716-449955440097", "-aName", "Test", "-aValue", "Test", "-hn", "localhost:8080" };
        RESTClient.main(args);
    }

    @Test
    public void testDeleteAdornment() throws IOException {
        String[] args = { "-delete", "-csid", "550e4713-e22b-11d4-a716-446655440002", "-cdid",
                "440e4816-e01b-74d4-a716-449955440097", "-aName", "Verantwortlicher", "-hn", "localhost:8080" };
        RESTClient.main(args);
    }

    @Test
    public void testPostPayload() throws IOException {
        String[] args = { "-add", "-csid", "550e4713-e22b-11d4-a716-446655440002", "-cdid",
                "440e4816-e01b-74d4-a716-449955440097", "-pName", "Test.desktop", "-path",
                "/home/i6stud/ew28uwuw/Desktop/Eclipse (SWAT).desktop", "-hn", "localhost:8080" };
        RESTClient.main(args);
    }

    @Test
    public void testGetPayload() throws IOException {
        String[] args = { "-get", "-csid", "550e4713-e22b-11d4-a716-446655440002", "-cdid",
                "440e4816-e01b-74d4-a716-449955440097", "-pName", "Test.desktop", "-hn", "localhost:8080" };
        RESTClient.main(args);
    }
}
