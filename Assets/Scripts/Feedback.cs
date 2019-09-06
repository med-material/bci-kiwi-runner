using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Feedback : MonoBehaviour
{
    private AudioSource audiofb;
    
    void Start()
    {
        audiofb = GetComponent<AudioSource>();
    }
    
    public void Play()
    {
        audiofb.Play(); //play audio
    }

    /*public void Sham()
    {
        rnd = Random.Range(1, 100); //random number for sham feedback
        Debug.Log("s: " + rnd);
        if (rnd <= shamRate) //if the random number is lower than or equal to the shamrate, do sham feedback
        { 
            sham = true;
            shamCounter++;
        }
        else
            sham = false;
    }*/
}