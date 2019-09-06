using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class SpawnObstacles : MonoBehaviour
{

    public GameObject[] obstacles;
    public GameObject babies;

    private float spawnPosX;
    private float spawnPosY;
    private Vector3 spawnPoint; //obstacle spawnpoint


    public static float spawnTime; //the time between spawns
    public static bool timeToSpawn; //checks if it is time to spawn

    private int trials;
    public int maxTrials = 12;
    
    void Start() //set spawntime at beginning
    {
        spawnTime = 0;

        Transform ground = gameObject.transform;
        spawnPosY = ground.position.y + ground.localScale.y / 2;
        spawnPosX = ground.position.x + ground.localScale.x / 2.5f;
        spawnPoint = new Vector3(spawnPosX, spawnPosY, 0);

        timeToSpawn = true;
    }
    
    void Update() 
    {
        if (Avatar.start && trials < maxTrials)
        {
            if (spawnTime <= 0 && timeToSpawn) //when spawntime hits 0, and we're in the ITT, spawn obstacle
            {
                int rnd = Random.Range(0, obstacles.Length-1);
                Spawn(obstacles[rnd], spawnPoint);
            }
            else if (spawnTime > 0) //otherwise, countdown to spawntime
                spawnTime -= Time.deltaTime;
        }
        else if(trials == maxTrials && timeToSpawn)
        {
            Vector3 spawnPoint2 = new Vector3(spawnPosX / 2, spawnPosY, 0);
            Spawn(babies, spawnPoint2);
        }
    }

    void Spawn(GameObject obs, Vector3 spawnPoint)
    {
        Instantiate(obs, spawnPoint, Quaternion.identity); //spawn new random obstacle
        timeToSpawn = false;
        trials++;
    }
}
