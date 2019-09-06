using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ObstacleBabies : MonoBehaviour
{
    private Rigidbody2D rb;
    private float groundSize;
    private Animator anim;

    void Start()
    {
        rb = gameObject.GetComponent<Rigidbody2D>();
        groundSize = GameObject.Find("Ground Quad").GetComponent<Transform>().localScale.x;
        anim = gameObject.GetComponent<Animator>();

        Avatar.endgame = true;
    }

    void Update()
    {
        if (Avatar.start)
        {
            rb.transform.Translate(Avatar.speed * groundSize, 0, 0);
        }
        
        if (gameObject.GetComponentInChildren<Transform>().Find("OnScreen").GetComponent<SpriteRenderer>().isVisible)
        {
            Avatar.start = false;
        }
    }

    private void OnTriggerEnter2D(Collider2D other)
    {
        if (other.tag == "Player")
            anim.enabled = true;
    }
}