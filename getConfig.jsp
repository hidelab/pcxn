<%@page contentType="text/html; charset=UTF-8"%>
<%@page import="java.io.FileNotFoundException"%>
<%@page import="java.io.IOException"%>
<%@page import="java.io.InputStream"%>
<%@page import="java.util.Date"%>
<%@page import="java.util.Properties"%>
<html>
	<head>
		<title>Get Configs</title>
	</head>
	<body>
		<h2>Get Configs</h2>
		<%
			String user = "";
			String password = "";
			try {
				Properties prop = new Properties();
				String propFileName = "pcxnconfig.properties"; 
				InputStream inputStream = getClass().getClassLoader().getResourceAsStream(propFileName); 
				if (inputStream != null) {
					prop.load(inputStream);
				} else {
					throw new FileNotFoundException("property file '" + propFileName + "' not found in the classpath");
				}
				user = prop.getProperty("user");
				password = prop.getProperty("password");
			} catch (Exception e) {
				out.println("Exception: " + e);
			} finally {
			}
			out.println("username: " + user);
			out.println("<br>");
			out.println("password: " + password);
		%>
		<br/>
		The current date and time is <%= new java.util.Date() %>
	</body>
</html>