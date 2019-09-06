using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ScrollScript : MonoBehaviour
{
    [Range(0.0f,1.0f)]
    public float speedMult;
    
    float offset = 0;
    
    void Update()
    {
        if(Avatar.start)
        {
            offset -= (Avatar.speed * speedMult);
            GetComponent<Renderer>().material.mainTextureOffset = new Vector2(offset, 0f);
        }
    }
}