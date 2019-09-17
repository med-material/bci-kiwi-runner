using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Obstacle : MonoBehaviour
{
    private Renderer rend;
    private float xPos;
    private float groundSize;
    
    void Start()
    {
        rend = GetComponentInChildren<Transform>().Find("SlowZone").GetComponentInChildren<SpriteRenderer>();
        groundSize = GameObject.Find("Ground Quad").GetComponent<Transform>().localScale.x;
    }

    void Update()
    {
        xPos = gameObject.transform.position.x;
        GetComponent<Rigidbody2D>().transform.Translate(Avatar.speed * groundSize, 0, 0);

        if (xPos < -20) //if obstacle has gone off camera, destroy it
        {
            Destroy(gameObject);
        }
    }

    private void OnTriggerExit2D(Collider2D other)
    {
        if (other.tag == "Player")
        {
            Physics2D.IgnoreCollision(other, GetComponent<Collider2D>()); //no collision for player
        }
    }
}
