package opmodes;

import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.TouchSensor;

import opmodes.calibration.MiyazakiCalibration;
import team25core.DeadReckonTask;
import team25core.DeadmanMotorTask;
import team25core.GamepadTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.RobotTask;
import team25core.RunToEncoderValueTask;
import team25core.SingleShotTimerTask;
import team25core.TouchSensorCriteria;

/**
 * Created by Lizzie on 2/8/2020.
 */
public class ElevatorTask extends RobotTask {

    protected int currentLevel = 0;
    protected int targetLevel = 1;
    protected int levelEncoderValue = 300;
    protected boolean fullyRetracted = true;
    protected boolean resetLevel = false;
    protected DcMotor vLift;
    protected TouchSensor touchSensor;
    private TouchSensorCriteria touchCriteria;

    public ElevatorTask(Robot robot, DcMotor vLift, TouchSensor touchSensor) {
        super(robot);
        this.vLift = vLift;
        this.touchSensor = touchSensor;
        vLift.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        vLift.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        touchCriteria = new TouchSensorCriteria(touchSensor);
    }

    @Override
    public void start ()
    {
    }

    @Override
    public void stop ()
    {

    }

    public void levelUp ()
    {
        if (fullyRetracted == true) {
            robot.addTask(new RunToEncoderValueTask(robot, vLift, levelEncoderValue * targetLevel, MiyazakiCalibration.VLIFT_UP) {
                public void handleEvent(RobotEvent e) {
                    RunToEncoderValueEvent event = (RunToEncoderValueEvent) e;
                    switch(event.kind) {
                        case DONE:
                            currentLevel = targetLevel;
                            if (targetLevel == 4) {
                                targetLevel = 1;
                            } else {
                                targetLevel++;
                            }
                            fullyRetracted = false;
                            break;
                    }
                }
            });
        }
    }

    public int getCurrentLevel()
    {
        return currentLevel;
    }

    public int getTargetLevel()
    {
        return targetLevel;
    }

    public void fullyRetractLift ()
    {
        resetLevel = true;
    }

    public void setTargetLevel(int newTargetLevel)
    {
        // set to certain level based on number of button presses?
        targetLevel = newTargetLevel;
    }

    @Override
    public boolean timeslice()
    {
        if (resetLevel == true) {
            vLift.setPower(MiyazakiCalibration.VLIFT_DOWN);
            if (touchSensor.isPressed() == true) {
                fullyRetracted = true;
                resetLevel = false;
                vLift.setPower(0.0);
                currentLevel = 0;
            }
        }
        return false;
    }
}
