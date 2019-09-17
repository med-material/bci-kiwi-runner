using System.Collections;
using System.Collections.Generic;
using System.Linq;
using UnityEngine;

public class Avatar : MonoBehaviour
{
    private Rigidbody2D rb;
    private Animator anim;

    private AudioSource music;
    private AudioSource windSound;
    private AudioSource birdCall;
    private AudioSource kiwiSquawk;
    private AudioSource feedback;

    private bool inTask; //checks if player is within task phase

    private float jumpForce;
    
    public SpriteRenderer signal;
    public SpriteRenderer prep;
    public SpriteRenderer exclamation;

    public static bool canDestroy;
    private bool end;

    public static bool doneDidDoIt;
    public static bool jumping;
    public static string zone = " ";

    public GameObject babies;
    public GameObject heart;
    
    private bool playSquawk = true;

    bool sham;
    
    private bool canStart;
    public static bool start;
    public static bool endgame;

    public static float speed; //current speed of environment moving
    public static float baseSpeed = -0.001f; //base speed

    [Range(0,2)]
    public int condition;

    private int[] cond0 = new int[20] { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 }; //0
    private int[] cond1 = new int[20] { 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 }; //1
    private int[] cond2 = new int[20] { 0, 0, 0, 0, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 }; //2
    private int[] demo = new int[10] { 0, 0, 2, 2, 2, 1, 1, 1, 1, 1 };
    private int[] input;
    
    int i;

    void Start()
    {
        zone = "prestart";
        speed = baseSpeed; //set speed to base speed at first

        rb = GetComponent<Rigidbody2D>();
        anim = GetComponent<Animator>();
        jumpForce = 400f;

        AudioSource[] soundEffects = GetComponents<AudioSource>();
        windSound = soundEffects[0];
        birdCall = soundEffects[1];
        music = soundEffects[2];
        kiwiSquawk = soundEffects[3];
        feedback = soundEffects[4];

        if (condition == 0)
            input = cond0;
        else if (condition == 1)
            input = cond1;
        else if (condition == 2)
            input = cond2;

        input = Shuffle(input);

        Invoke("OhNo", 2.0f);
        Invoke("StartGame", 4.0f);
    }
    
    void Update()
    {
        if ((Input.GetKeyDown(KeyCode.Space) || BlinkDetector.blinked) && !start && canStart)
        {
            zone = "ground";
            start = true;

            exclamation.enabled = false;

            Show(babies, false, false);

            anim.Play("kiwiRun");
            signal.enabled = false;
            music.Play();
            canStart = false;
        }
        
        if (inTask && (BlinkDetector.blinked || Input.GetKeyDown(KeyCode.UpArrow)))
        {
            doneDidDoIt = true;
            Invoke("StopDoinIt", 0.1f);

            Task(input[i]);
            inTask = false;
        }
        
        if(!end && endgame)
        {
            if (playSquawk)
            {
                kiwiSquawk.Play();
                playSquawk = false;
            }
            exclamation.enabled = true;
            rb.transform.Translate((-speed/1.5f) * GameObject.Find("Ground Quad").GetComponent<Transform>().localScale.x, 0, 0);
        }
    }

    void StopDoinIt()
    {
        doneDidDoIt = false;
    }

    void StartGame()
    {
        signal.enabled = true;
        canStart = true;
    }

    void OhNo()
    {
        exclamation.enabled = true;
        Show(babies, true, true);
        kiwiSquawk.Play();
    }

    public void Task(int input)
    {
        if (input == 1) //if has completed task, jump and reset success
        {
            Jump();
            i++;
        }
        else if (input == 2)
        {
            i++;
            sham = true;
            float r = Random.Range(0.5f, 4.0f);
            Invoke("Jump", r);
        }
        else
            i++;

        //Debug.Log(input);
    }

    private int[] Shuffle(int[] myArray)
    {
        System.Random r = new System.Random();
        myArray = myArray.OrderBy(x => r.Next()).ToArray();

        string arr = "";
        foreach (int i in myArray)
            arr += i;

        //Debug.Log(arr);
        return myArray;
    }

    private void Jump() //when jumping, play feedback
    {
        jumping = true;

        rb.AddForce(Vector2.up * jumpForce);
        feedback.Play();
        anim.Play("kiwiJump");

        sham = false;
    }

    private void Show(GameObject toShow, bool renderer, bool animator)
    {
        toShow.GetComponent<SpriteRenderer>().enabled = renderer;
        toShow.GetComponent<Animator>().enabled = animator;
    }

    private void OnTriggerEnter2D(Collider2D other)
    {
        switch (other.tag)
        {
            case "Cue":
                prep.enabled = true;
                zone = "cue";
                break;

            case "TriggerZone":
                prep.enabled = false;
                signal.enabled = true;
                
                zone = "task";

                inTask = true;
                if (SpawnObstacles.trials % 5 != 0)
                {
                    //SpawnObstacles.spawnTime = 1f;
                    SpawnObstacles.timeToSpawn = true;
                }

                break;

            case "Slow":
                speed *= 0.5f;
                anim.speed *= 0.5f;
                Show(babies, true, true);

                zone = "slow";
                break;

            case "Sham":
                if (sham)
                {
                    zone = "sham";
                    Jump();
                }

                sham = false;
                CancelInvoke();
                break;

            case "Flying":
                zone = "flying";

                jumping = false;

                rb.velocity = Vector2.zero;
                rb.gravityScale = 0;

                speed *= 2;

                anim.Play("kiwiFall");
                windSound.Play();
                break;

            case "Babies":
                anim.Play("kiwiIdle");
                music.Stop();
                birdCall.Play();

                end = true;
                exclamation.enabled = false;

                zone = "babies";

                Show(heart, true, true);
                break;

            default:
                break;
        }
    }

    private void OnTriggerExit2D(Collider2D other)
    {
        switch (other.tag)
        {
            case "TriggerZone":
                inTask = false;
                signal.enabled = false;
                break;

            case "Slow":
                speed = baseSpeed;
                if(SpawnObstacles.trials % 5 == 0) {
                    //SpawnObstacles.spawnTime = 1f;
                    SpawnObstacles.timeToSpawn = true;
                }

                Show(babies, false, false);
                zone = "ground";
                anim.speed *= 2;
                canDestroy = true;

                SpawnObstacles.trials++;

                break;

            case "Flying":
                speed = baseSpeed;
                if (SpawnObstacles.trials % 5 == 0)
                {
                    //SpawnObstacles.spawnTime = 1f;
                    SpawnObstacles.timeToSpawn = true;
                }

                rb.gravityScale = 1;
                anim.Play("kiwiJump");
                SpawnObstacles.trials++;

                canDestroy = true;
                zone = "ground";
                windSound.Stop();
                break;

            default:
                break;
        }
    }

    private void OnCollisionEnter2D(Collision2D other)
    {
        if (other.gameObject.CompareTag("Ground"))
        {
            if (start)
                anim.Play("kiwiRun");
        }
    }
}






//Debug.Log("a: " + rnd);
/*if (rnd < accuracy && !success)
    success = true;
else
    shamTime = true;*/

/*if (condition == 0) {
    Task(cond0[i], false);
}
else if (condition == 1)
{
    if (cond0[i] == true)
        Task(cond0[i], false);
    else
        Task(cond0[i], cond1[j]);
}
else if (condition == 2)
{
    if (cond0[i] == true)
        Task(cond0[i], false);
    else
        Task(cond0[i], cond2[j]);
}*/



/*else if (shamTime) //if player reached shamzone (i.e. didn't succeed), initiate sham function and jump if true
{
    fb.Sham();
    shamTime = false;

    if (fb.sham == true)
    {
        float rnd = Random.Range(0.5f, 4.0f);
        Invoke("Jump", rnd);
    }
}*/
