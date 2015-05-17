using UnityEngine;
using System.Collections;
using System.Net.Sockets;
using SocketIOClient;
using SocketIOClient.Messages;
using SocketIOClient.Eventing;
using System;
using System.Text;

public class client : MonoBehaviour {


	Int32 Port = 8889;
	TcpClient Client;
	NetworkStream stream;

	private void SocketOpened(object sender, MessageEventArgs e) {
		//invoke when socket opened
	}
	// Use this for initialization
	void Start () {		
		Client = new TcpClient("127.0.0.1", Port);
		stream = Client.GetStream();
		Byte[] data = System.Text.Encoding.ASCII.GetBytes("join"); 
		
		// Send the message to the connected TcpServer. 
		stream.Write(data, 0, data.Length);
		
		Debug.Log("Sent: join");         
		
		// Receive the TcpServer.response. 
		
		// Buffer to store the response bytes.
		data = new Byte[256];
		
		// String to store the response ASCII representation.
		String responseData = String.Empty;
		
		// Read the first batch of the TcpServer response bytes.
		Int32 bytes = stream.Read(data, 0, data.Length);
		responseData = System.Text.Encoding.ASCII.GetString(data, 0, bytes);
		Debug.Log("Received: " + responseData);         
		       

	}

	void Update () {
		string pos = GameObject.Find("ball").transform.position.ToString();
		byte[] data = Encoding.UTF8.GetBytes(pos);
		
		// Send the message to the connected TcpServer. 
		stream.Write(data, 0, data.Length);
	}

	void OnDestroy () {
		
		// Close everything.
		stream.Close();         
		Client.Close();  
	}

}
