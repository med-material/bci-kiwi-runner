using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Events;



[System.Serializable]
public class MyEvent : UnityEvent<Dictionary<string, List<string>>>
{

}

public class Lognotifier : MonoBehaviour
{

    public MyEvent m_MyEvent;
    // Start is called before the first frame update
    void Start()
    {

    }

    // Update is called once per frames
    protected virtual void notify(Dictionary<string, List<string>> dico)
    {

        m_MyEvent.Invoke(dico);

    }
}
