using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Tobii.Gaming;

public class BlinkDetector : MonoBehaviour
{
    public static bool blinked;

    void Update()
    {
        if (!TobiiAPI.GetGazePoint().IsRecent(0.1f))
        {
            blinked = true;
        }
        else
        {
            blinked = false;
        }
    }
}