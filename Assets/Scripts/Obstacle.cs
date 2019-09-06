using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Obstacle : MonoBehaviour
{
    private Renderer rend;
    private float groundSize;

    void Start()
    {
        rend = GetComponentInChildren<Transform>().Find("SlowZone").GetComponentInChildren<SpriteRenderer>();
        groundSize = GameObject.Find("Ground Quad").GetComponent<Transform>().localScale.x;
    }

    void Update()
    {
        GetComponent<Rigidbody2D>().transform.Translate(Avatar.speed * groundSize, 0, 0);

        if (Avatar.canDestroy && !rend.isVisible) //if obstacle has gone off camera, destroy it
        {   
            Avatar.canDestroy = false;
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
