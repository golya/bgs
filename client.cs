using UnityEngine;
using System.Collections;
using System.Net.Sockets;
using SocketIOClient;
using SocketIOClient.Messages;
using SocketIOClient.Eventing;
using System;
using System.Text;
using System.Threading;
using System.IO;

public class Client : MonoBehaviour {

	public NetworkStream stream;
	Int32 Port = 8889;
	byte[] data = new byte[1024];
	string receiveMsg = "";
	bool conReady = false;
	TcpClient client;

	// Use this for initialization
	void Start () {		
		client = new TcpClient("127.0.0.1", Port);
		stream = client.GetStream();		
		Debug.Log("connect to server");       
		conReady = true;
		       

	}

	public void sendMsg(float x, float y) {
		byte[] data = System.Text.Encoding.ASCII.GetBytes(x.ToString() + "," +  y.ToString());
		stream.Write (data, 0, data.Length);
	}

	void Update () {
		if(client.Connected) {
			receiveData();
		}		 
	}

	public void receiveData() {
		if(!conReady) {
			Debug.Log("connection not ready...");
			return;
		}
		
		int numberOfBytesRead = 0;
		
		if(stream.CanRead) {
			try {
				
				numberOfBytesRead = stream.Read(data, 0, data.Length);  
				receiveMsg = System.Text.Encoding.ASCII.GetString(data, 0, numberOfBytesRead);
				Debug.Log("receive msg:  " + receiveMsg);
				string[] words = receiveMsg.Split(',');

				Vector3 dir = Vector3.zero;
				dir.y = float.Parse(words[1], System.Globalization.CultureInfo.InvariantCulture);
				dir.x = float.Parse(words[0], System.Globalization.CultureInfo.InvariantCulture);
				if (dir.sqrMagnitude > 1)
					dir.Normalize();
				
				dir *= Time.deltaTime;
				GameObject.Find("ball").transform.Translate(dir * 10);


			}
			catch(Exception e)
			{
				Debug.Log("Error in NetworkStream: " + e);
			}
		}
		
		receiveMsg = "";
	}


	void OnDestroy () {
		
		// Close everything.
		stream.Close();         
		client.Close();  
	}

}
