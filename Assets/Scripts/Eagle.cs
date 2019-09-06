using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Eagle : MonoBehaviour
{
    Vector2 velocity = new Vector2(6.5f, 0);
    public SpriteRenderer exclamation;
    
    void Start()
    {
        Invoke("PlaySound", 1.5f);
    }
    
    void Update()
    {
        gameObject.GetComponent<Rigidbody2D>().velocity = velocity;

        if (Avatar.start && !Avatar.endgame)
        {
            velocity = Vector2.zero;
            Bye();
        }

        if (Avatar.endgame && gameObject.GetComponent<SpriteRenderer>().isVisible)
        {
            velocity = new Vector2(1.5f, -0.5f);
            Invoke("Whoops", 5);
            Invoke("FlyAway", 6);
        }
    }

    void Whoops()
    {
        velocity = new Vector2(0, 0);
        exclamation.enabled = true;
    }

    void FlyAway()
    {
        velocity = new Vector2(5.0f, 1.0f);
        Invoke("Bye", 3);
    }

    void PlaySound()
    {
        gameObject.GetComponent<AudioSource>().Play();
    }

    void Bye()
    {
        Destroy(gameObject);
        CancelInvoke();
    }
}